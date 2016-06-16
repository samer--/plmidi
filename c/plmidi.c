/*
 *  plmidi - MIDI output for SWI Prolog on Mac OS X
 *
 *  Copyright (C) 2009 Samer Abdallah
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 */

// todo:
// 	timebase_freq as rational number for better accuracy
// 	fix engine leak

#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <mach/mach_time.h>
#include <mach/mach_port.h>
#include <mach/mach_interface.h>
#include <mach/mach_init.h>
#include <CoreMIDI/MIDIServices.h>
#include <IOKit/pwr_mgt/IOPMLib.h>
#include <IOKit/IOMessage.h>


#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <sys/time.h>

// data pertaining to a CoreMIDI connection
typedef struct midi_conn {
	MIDIPortRef	  		port;
	MIDIEndpointRef	endpt;
	PL_engine_t			eng; // only used by inlets
} midi_conn_t;

// for representing a CoreMIDI connection as an atom
static PL_blob_t conn_blob;

// for mapping Unix time to CoreMIDI timestamps
static double timebase_origin=0;
static double timebase_freq=0;
static mach_timebase_info_data_t timebase;

// for wake/sleep notification
static io_connect_t  root_port; 

// for calling MIDI input handler goal
static predicate_t call3;
static functor_t f_midi[3];

// single shared client
static MIDIClientRef midi_client;

// single shared engine for servicing MIDI input events
static PL_engine_t input_engine=NULL;

static PL_engine_t get_singleton_engine()
{
	if (input_engine==NULL) {
		printf("plmidi: creating input callback Prolog engine.\n");
		input_engine=PL_create_engine(NULL);
	}
	return input_engine;
}

// --------------------------   Time conversion

static double timestamp_to_prolog_time(MIDITimeStamp ts) {
	return timebase_origin + ts/timebase_freq; 
}


// convert gettimeofday() timeval to microseconds since 1970
static uint64_t timeval_to_micros(struct timeval *tv) {
	return (uint64_t)tv->tv_sec*1000000 + tv->tv_usec;
}

// convert mach system time to microseconds 
static double machtime_to_micros(uint64_t abstime) {
	return abstime*((double)timebase.numer/(double)timebase.denom)/1000.0;
}

// establish relationship between Unix time and mach system time
static void calibrate_timer(int verbose) 
{
	uint64_t mt0, mt1, mt2, tc1;
	double   tm0, tm1, tm2;
	double   off1, off2, moff;
	struct  timeval ct1;

	// Sdprintf("\n-- plmidi: calibrating MIDI timebase...\n");
	mach_timebase_info(&timebase); // gets conversion factor as a rational number
	// printf("mach timebase: %u/%u\n",timebase.numer,timebase.denom);

	mt0=mach_absolute_time();
	gettimeofday(&ct1,NULL);
	mt1=mach_absolute_time();
	mt2=mach_absolute_time();

	tm0=machtime_to_micros(mt0);
	tm1=machtime_to_micros(mt1);
	tm2=machtime_to_micros(mt2); 
	tc1=timeval_to_micros(&ct1);

	if (verbose) {
		printf("mach_absolute_time() duration = %llu ticks = %lf us\n",mt2-mt1,tm2-tm1);
		printf("gettimeofday()       duration = %llu ticks = %lf us\n",(mt1-mt0)-(mt2-mt1),(tm1-tm0)-(tm2-tm1));
		printf("\n");

		printf("mach time = %lf us\n",tm1);
		printf("unix time = %lld us\n",tc1);
		printf("\n");
	}

	// compute difference between Unix time and mach time in 
	// two ways using measurements taken above, then take mean
	off1=tc1-tm0; off2=tc1-tm1; 
	moff=(off1+off2)/2; 

	timebase_origin = moff/1000000;                    
	timebase_freq = 1e9*timebase.denom/timebase.numer; 

	if (verbose) {
		printf("timebase origin = %lf s\n",timebase_origin);
		printf("timebase freq   = %lf Hz\n",timebase_freq);
	}
	//printf("-- plmidi: done calibrating MIDI timebase.\n\n");
}
	


// --------------------------   Wake notification


void sleep_cb(void *refCon, io_service_t service, natural_t msg_type, void * msg_arg )
{
    switch (msg_type) {
        case kIOMessageCanSystemSleep:
        case kIOMessageSystemWillSleep:
            IOAllowPowerChange( root_port, (long)msg_arg);
            break;

        case kIOMessageSystemHasPoweredOn: 
				calibrate_timer(0); 
				break;

        default: break;
    }
}


void *install_sleep_handler(void *dummy)
{
    IONotificationPortRef  notifyPortRef; // notification port allocated by IORegisterForSystemPower
    io_object_t            notifierObject; // notifier object, used to deregister later
    void*                  refCon=(void *)0; // this parameter is passed to the callback

    // register to receive system sleep notifications

	 printf("\n-- registering plmidi wake/sleep handler...\n");
    root_port = IORegisterForSystemPower( refCon, &notifyPortRef, sleep_cb, &notifierObject );
    if (root_port==0) {
        printf("*** IORegisterForSystemPower failed\n");
        printf("*** You must call midi_calibrate manually after system sleep\n");
		  return NULL;
    }

    // add the notification port to the application runloop
    CFRunLoopAddSource( CFRunLoopGetCurrent(),
            IONotificationPortGetRunLoopSource(notifyPortRef), kCFRunLoopCommonModes ); 

	 printf("-- wake/sleep handler thread running...\n\n");
    CFRunLoopRun();
	 printf("-- wake/sleep handler thread terminating.\n");
    return NULL;
}



// --------------------------   MIDI API


// establish a connection to a MIDI destination and fill midi_conn_t
// structure with relevant handles.
static int open_outlet(int id, midi_conn_t *outlet)
{
	outlet->endpt = 0;
	outlet->port = 0;
	outlet->eng  = 0;


	MIDIOutputPortCreate(midi_client, CFSTR("plmidi_out"), &outlet->port);

	if (outlet->port!=0) {
		if (id<MIDIGetNumberOfDestinations()) outlet->endpt=MIDIGetDestination(id);
		if (outlet->endpt!=0) {
			CFStringRef pname;
			char name[64];

			MIDIObjectGetStringProperty(outlet->endpt, kMIDIPropertyName, &pname);
			CFStringGetCString(pname, name, sizeof(name), 0);
			CFRelease(pname);
			printf("MIDI outlet opened: %s\n", name);
			return TRUE;
		} else {
			MIDIPortDispose(outlet->port);
			return FALSE;
		}
	} else {
		return FALSE;
	}
}

static int close_conn(midi_conn_t *conn) 
{
	if (conn->eng!=0) MIDIPortDisconnectSource(conn->port,conn->endpt);
	if (conn->port!=0) MIDIPortDispose(conn->port);
	return TRUE;
}
	
int chomp(const unsigned char status, unsigned short *ok, unsigned short *size) 
{
	// We are expecting that the next byte in the packet is a status byte.
	if ( !(status & 0x80) ) return 1; // abort this packet

	*ok=1; // default is to transmit message
	// Determine the number of bytes in the MIDI message.
	if      (status<0xC0) *size=3; 
	else if (status<0xE0) *size=2;
	else if (status<0xF0) *size=3;
	else {
		switch (status) {
			case 0xF0: return 1; // sys ex: abort packet
			case 0xF1: *size=3; *ok=0; break; // time code: ignore
			case 0xF2: *size=3; break;
			case 0xF3: *size=2; break;
			case 0xF8: *size=3; *ok=0; break; // timing tick: ignore
			case 0xFE: *size=1; *ok=0; break; // active sensing: ingore
			default:   *size=1; 
		}
	}
	return 0;
}


void plmidi_read(const MIDIPacketList *pktlist, void *portData, void *srcData)
{
	PL_engine_t eng = (PL_engine_t)portData;

	PL_set_engine(eng,NULL);
	{
		term_t goal = PL_new_term_ref();

		PL_recorded((record_t)srcData,goal); // retrieve the goal term
		{ // most of this was cribbed from RtMidi C++ package
			const MIDIPacket *packet = &pktlist->packet[0];
			unsigned int  i;

			for (i=0; i<pktlist->numPackets; ++i ) {
				const unsigned char *pdata=packet->data;
				unsigned short nBytes=packet->length;
				unsigned short iByte;
				unsigned short size;
				double time=timestamp_to_prolog_time(packet->timeStamp);

				// decode packet->timeStamp to Prolog time

				for (iByte=0; iByte<nBytes; iByte+=size) {
					unsigned short transmit;
					if (chomp(pdata[iByte],&transmit,&size)) break;
					if (transmit) {
						term_t data0 = PL_new_term_refs(size);
						term_t term0 = PL_new_term_refs(3);
						term_t term1 = term0+1;
						term_t term2 = term0+2;
						int i;

						for (i=0; i<size; i++) PL_put_integer(data0+i,pdata[iByte+i]);

						PL_put_term(term0,goal); 
						PL_put_float(term1,time);
						PL_cons_functor_v(term2,f_midi[size-1],data0);

						// data is from pdata[iByte] to pdata[iByte+size]
						PL_call_predicate(NULL,PL_Q_NORMAL,call3,term0);
					} 
				}
				packet = MIDIPacketNext(packet);
			}
		}
	}
	PL_set_engine(NULL,NULL);
}

// establish a connection to a MIDI destination and fill midi_conn_t
// structure with relevant handles.
static int open_inlet(int id, midi_conn_t *inlet, term_t handler)
{
	inlet->endpt = 0;
	inlet->port = 0;
	inlet->eng = get_singleton_engine();
	PL_create_engine(NULL);

	MIDIInputPortCreate(midi_client, CFSTR("plmidi_in"), plmidi_read, inlet->eng, &inlet->port);

	if (inlet->port!=0) {
		if (id<MIDIGetNumberOfSources()) inlet->endpt=MIDIGetSource(id);
		if (inlet->endpt!=0) {
			record_t	goal_record=PL_record(handler);
			OSStatus rc=MIDIPortConnectSource(inlet->port, inlet->endpt, (void *)goal_record);

			if (rc==0) { // everything ok
				CFStringRef pname;
				char name[64];

				MIDIObjectGetStringProperty(inlet->endpt, kMIDIPropertyName, &pname);
				CFStringGetCString(pname, name, sizeof(name), 0);
				CFRelease(pname);
				printf("MIDI inlet opened: %s\n", name);
				return TRUE;
			} else {
				MIDIEndpointDispose(inlet->endpt);
				MIDIPortDispose(inlet->port);
				return FALSE;
			}
		} else {
			printf("source not available.\n");
			MIDIPortDispose(inlet->port);
			return FALSE;
		}
	} else {
		printf("could not open port.\n");
		return FALSE;
	}
}

static void get_string_property(MIDIEndpointRef dev, CFStringRef prop, char *buf, int len)
{
	CFStringRef str;
	MIDIObjectGetStringProperty(dev, prop, &str);
	if (str) { CFStringGetCString(str, buf, len, 0); CFRelease(str); }
	else { strcpy(buf,"<null>"); }
}

static int unify_endpoint_info(term_t term, char *func, int i, MIDIEndpointRef dev)
{
	char name[64], manuf[64], model[64];

	get_string_property(dev, kMIDIPropertyName, name, sizeof(name));
	get_string_property(dev, kMIDIPropertyManufacturer, manuf, sizeof(manuf));
	get_string_property(dev, kMIDIPropertyModel, model, sizeof(model));

	return PL_unify_term(term, PL_FUNCTOR_CHARS, func, 4,
			PL_INT, i, PL_CHARS, name, PL_CHARS, manuf, PL_CHARS, model);
}


// send a MIDI message
static int send_msg(
		midi_conn_t *outlet, MIDITimeStamp ts, 
		unsigned char msg, unsigned char arg1, unsigned char arg2)
{
	MIDIPacketList pktlist;
	MIDIPacket     *pkt = &pktlist.packet[0];

	pktlist.numPackets = 1;
	pkt->timeStamp = ts;
	pkt->length = 3;
	pkt->data[0] = msg;
	pkt->data[1] = arg1;
	pkt->data[2] = arg2;

	MIDISend(outlet->port, outlet->endpt, &pktlist);
	return TRUE;
}



// --------------------------   Prolog boilerplate

install_t install();

foreign_t endpoints(); 
foreign_t calibrate();
foreign_t mk_outlet( term_t id, term_t outlet); 
foreign_t mk_inlet( term_t id, term_t inlet, term_t handler); 
foreign_t is_conn( term_t conn); 
foreign_t send_midi_now( term_t addr, term_t msg, term_t arg1, term_t arg2); 
foreign_t send_midi_at( term_t addr, term_t msg, term_t arg1, term_t arg2, term_t time); 

int conn_release(atom_t a)
{
	PL_blob_t *type;
	size_t    len;
	void *p;

	p=PL_blob_data(a,&len,&type);
	if (p) {
		printf("MIDI connection <%p> closing...\n",p);
		close_conn((midi_conn_t *)p);
	}
	return TRUE;
}

static void midi_notify_cb(const MIDINotification *msg, void *ref) {
	printf("\n*** plmidi: received notification, id=%d.\n",msg->messageID);
}

install_t install() 
{ 
	PL_register_foreign("midi_mk_outlet",  2, (void *)mk_outlet, 0);
	PL_register_foreign("midi_mk_inlet",  3, (void *)mk_inlet, 0);
	PL_register_foreign("midi_is_conn",  1, (void *)is_conn, 0);
	PL_register_foreign("midi_send_now",   4, (void *)send_midi_now, 0);
	PL_register_foreign("midi_send_at",    5, (void *)send_midi_at, 0);
	PL_register_foreign("midi_endpoints",    1, (void *)endpoints, 0);
	PL_register_foreign("midi_calibrate",  0, (void *)calibrate, 0);

	conn_blob.magic = PL_BLOB_MAGIC;
	conn_blob.flags = PL_BLOB_UNIQUE;
	conn_blob.name = "plmidi_conn";
	conn_blob.acquire = 0; // rs_acquire;
	conn_blob.release = conn_release;
	conn_blob.compare = 0; // rs_compare;
	conn_blob.write   = 0; // rs_write;

	call3 = PL_predicate("call",3,"user");
	{
		atom_t a_midi = PL_new_atom("midi");
		int i;
		for (i=0; i<3; i++) f_midi[i] = PL_new_functor(a_midi,i+1);
	}

	calibrate_timer(1);
	{
		pthread_t tid;
		pthread_create(&tid, NULL, install_sleep_handler, NULL);
		pthread_detach(tid);
	}

	MIDIClientCreate(CFSTR("plmidi"), midi_notify_cb, NULL, &midi_client);
}

	

	
// throws a Prolog exception to signal type error
static int type_error(term_t actual, const char *expected)
{ 
	term_t ex = PL_new_term_ref();

  return PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "type_error", 2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE)
	  && PL_raise_exception(ex);
}

/*
static int midi_error(int errno, const char *errmsg, const char *msg)
{ 
	term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "midi_send_error", 3,
		        PL_INTEGER, errno,
		        PL_CHARS, errmsg,
		        PL_CHARS, msg,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}
*/

// put midi_conn_t data in a Prolog BLOB 
static int unify_conn(term_t conn,midi_conn_t *p) {
	return PL_unify_blob(conn, p, sizeof(midi_conn_t), &conn_blob); 
}

// get midi_conn_t data from a Prolog BLOB
static int get_conn(term_t conn, midi_conn_t *p)
{ 
	PL_blob_t *type;
	size_t    len;
	midi_conn_t *p1;
  
	PL_get_blob(conn, (void **)&p1, &len, &type);
	if (type != &conn_blob) {
		return type_error(conn, "plmidi_conn");
	} else {
		*p=*p1;
		return TRUE;
	}
} 

// get Prolog (Unix) time from a term (should be floating point number)
static int get_prolog_time(term_t time, MIDITimeStamp *ts)
{
	double t;

	if (PL_get_float(time, &t)) {
		*ts = (uint64_t)((t-timebase_origin)*timebase_freq); 
		return TRUE;
	} else {
		return type_error(time,"float");
	}
}

// get an unsigned byte from a numeric atom
static int get_byte(term_t msg, unsigned char *m)
{
	int x;
	if (!PL_get_integer(msg,&x) || x<0 || x>255) return type_error(msg,"uint8");
	*m = x;
	return TRUE;
}


// ------- Foreign interface predicates
//
foreign_t calibrate() { calibrate_timer(1); return TRUE; }

foreign_t endpoints(term_t list) { 
	int i, n;

	list=PL_copy_term_ref(list);

	n = MIDIGetNumberOfDestinations();
	for (i = 0; i < n; ++i) {
		term_t head=PL_new_term_ref();
		term_t tail=PL_new_term_ref();
		if (!PL_unify_list(list,head,tail)) PL_fail;
		if (!unify_endpoint_info(head,"outlet",i+1,MIDIGetDestination(i))) PL_fail;
		list=tail;
	}

	n = MIDIGetNumberOfSources();
	for (i = 0; i < n; ++i) {
		term_t head=PL_new_term_ref();
		term_t tail=PL_new_term_ref();
		if (!PL_unify_list(list,head,tail)) PL_fail;
		if (!unify_endpoint_info(head,"inlet",i+1,MIDIGetSource(i))) PL_fail;
		list=tail;
	}
	return PL_unify_nil(list);
}

foreign_t mk_outlet(term_t id, term_t outlet) 
{ 
	int x;

	if (PL_get_integer(id, &x)) {
		midi_conn_t o;
		printf("going to open midi destination %d\n",x);
		return open_outlet(x-1,&o) && unify_conn(outlet,&o);
	} else {
		return type_error(id,"integer");
	}
}

foreign_t mk_inlet(term_t id, term_t inlet, term_t handler) 
{ 
	int x;

	if (PL_get_integer(id, &x)) {
		midi_conn_t o;
		printf("going to open midi source %d\n",x);
		return open_inlet(x-1,&o, handler) && unify_conn(inlet,&o);
	} else {
		return type_error(id,"integer");
	}
}

foreign_t is_conn(term_t conn) 
{ 
	PL_blob_t *type;
	return PL_is_blob(conn,&type) && type==&conn_blob;
}


foreign_t send_midi_at(term_t outlet, term_t msg, term_t arg1, term_t arg2, term_t time) 
{
	midi_conn_t 	o;
	MIDITimeStamp  ts;
	unsigned char	m, a1, a2;

	return get_conn(outlet,&o) &&
			get_prolog_time(time,&ts) &&
			get_byte(msg, &m) &&
			get_byte(arg1, &a1) &&
			get_byte(arg2, &a2) &&
			send_msg(&o,ts,m,a1,a2);
}


foreign_t send_midi_now(term_t outlet, term_t msg, term_t arg1, term_t arg2)
{
	midi_conn_t o;
	unsigned char	m, a1, a2;

	return get_conn(outlet,&o) &&
			get_byte(msg, &m) &&
			get_byte(arg1, &a1) &&
			get_byte(arg2, &a2) &&
			send_msg(&o,0,m,a1,a2);
}

