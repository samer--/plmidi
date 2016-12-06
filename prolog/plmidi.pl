/*
 * Prolog part of random generator library
 * Samer Abdallah (2009)
*/
	  
:- module(plmidi, [
		midi_mk_outlet/2		% +N:natural, -Ref
	,	midi_mk_inlet/3		% +N:natural, -Ref, +Handler
	,	midi_is_conn/1			% +Ref
	,	midi/3     				% +Ref, +Time:float, +Event
	,	midi/2     				% +Time:float, +Event
	,	midi_endpoints/1	
	,	midi_endpoint/1	
	,	midi_calibrate/0

	,	midi_connect/1
   ,  midi_connect_named/1
	,	midinote/4
	,	midinote_at/5
	,	midi_outlet/2
	]).
	
:- meta_predicate midi_mk_inlet(+,-,2).
:- dynamic midi_outlet/2.

/** <module> MIDI event output

This module provides the ability to send MIDI events. It uses the
Mac OS X CoreMIDI framework. Events can be sent for immediate
dispatch or scheduled for future dispatch at a given time.
Times are specified in seconds since 1st Jan 1970, ie as returned
by get_time/1.

@author Samer Abdallah

@version 0.3
*/

:-	use_foreign_library(foreign(plmidi)).
:- use_module(library(mididcg)).

%% midi_endpoints( -L:list(endpoint)) is det.
%
%  Unify L with a list of all the available MIDI outlets and inlets
%  available on the system. Each endpoint is one of
%    * outlet( +Id:natural, +Name:atom, +Manuf:atom, +Model:atom)
%    * inlet( +Id:natural, +Name:atom, +Manuf:atom, +Model:atom)
%  The Id can be used with midi_mk_outlet/2 or midi_mk_inlet/3.

%% midi_endpoint( -E:endpoint) is nondet.
%
%  True if E is an endpoint on the system. Unifies E with all available
%  endpoints on backtracking.  Each endpoint is one of
%    * outlet( +Id:natural, +Name:atom, +Manuf:atom, +Model:atom)
%    * inlet( +Id:natural, +Name:atom, +Manuf:atom, +Model:atom)
%  The Id can be used with midi_mk_outlet/2 or midi_mk_inlet/3.
midi_endpoint(E) :- midi_endpoints(L), member(E,L).

%% midi_mk_outlet( +N:natural, -Ref) is semidet.
%
%  Opens a connection to the Nth CoreMIDI destination. Fails
%  if there are fewer than N destinations available. 
%  Available destinations can be obtained using 
%  midi_endpoints/1 or midi_endpoint/1. Resources associated with the connection
%  will be released when the Ref atom is reclaimed by the garbage
%  collector.
%
%  @param N is the index of the destination to open, 1 is the first.
%  @param Ref is a BLOB atom representing the connection.

%% midi_mk_inlet( +N:natural, -Ref, +Handler:pred(float,midimsg)) is semidet.
%
%  Opens a connection to the Nth CoreMIDI source. Fails
%  if there are fewer than N sources available. 
%  Available sources can be obtained using 
%  midi_endpoints/1 or midi_endpoint/1. Resources associated with the connection
%  will be released when the Ref atom is reclaimed by the garbage
%  collector.
%
%  @param N is the index of the destination to open, 1 is the first.
%  @param Ref is a BLOB atom representing the connection.
%  @param Handler is a callable goal to handle received messages,
%         of type pred( +Time:float, +Message:term). Message can be
%         midi(M), midi(M,A), or midi(M,A,B), depending on how much
%         data is associated with the message.

%% midi_is_conn(+Ref) is semidet.
%
%  Determines whether or not Ref is a MIDI connection BLOB as returned
%  by midi_mk_outlet/2 or midi_mk_inlet/3.




%% midi(+Ref, +Time:float, +Event) is semidet.
%
%  Schedule a MIDI event, possibly consisting of multiple messages, determined
%  by the Event term, which must be a callable predicate in library(mididcg).
%
%  @param Ref is an atom as returned by midi_mk_outlet/2.
%  @param Time is a Unix time as returnd by get_time/1.
%  @param Event is a term describing an event, and is one of:

midi(T,E) :- midi_outlet(_,O), midi(O,T,E).
midi(O,T,Ev) :- 
   once(call(mididcg:Ev, T-Msgs, _-[])),
   maplist(midi_send(O),Msgs).

% disable immediate messages!
% midi_send(O,imsg(M,A1))      :- M1 is M, B1 is A1, midi_send_now(O,M1,B1,255).
% midi_send(O,imsg(M,A1,A2))   :- M1 is M, B1 is A1, B2 is A2, midi_send_now(O,M1,B1,B2).
midi_send(O,msg(T,M,A1))    :- T1 is float(T), M1 is M, B1 is A1, midi_send_at(O,M1,B1,255,T1).
midi_send(O,msg(T,M,A1,A2)) :- T1 is float(T), M1 is M, B1 is A1, B2 is A2, midi_send_at(O,M1,B1,B2,T1).

%% midi_calibrate is det.
%
%  Establish a calibrated relationship between Unix time (seconds since
%  1st Jan 1970) and the timebase used by the CoreMIDI framework, which is
%  mach system time and is counted in bus cycles since system boot. The
%  relationship is not trivial since mach system time does not advance
%  while the system is asleep (eg a laptop is closed). When the module
%  is loaded, the calibration is done, and an IOKit wake/sleep event handler 
%  is installed. This should recalibrate the timer whenever the system
%  is awoken from sleep, but midi_calibrate/0 can be called at any time
%  just in case. 


%% midi_connect( +N:natural) is semidet.
%
%  Connect to MIDI device identified by index N (starting from 1).
%  This device is used for all subsequence MIDI output. Previous
%  connections are dropped.
midi_connect(Id) :-
	midi_mk_outlet(Id,Outlet),
	retractall(midi_outlet(_,_)),
	assert(midi_outlet(Id,Outlet)).

%% midi_connect_named(+String:atom) is det.
%  Connect to a MIDI output which contains String as part of its name.
%  Can throw =|named_midi_outlet_not_found(_)|= or =|named_midi_outlet_not_unique(_,_)|=.
midi_connect_named(String) :-
   findall(ID-Name, (midi_endpoint(outlet(ID,Name,_,_)), sub_atom(Name,_,_,_,String)), Matches),
   ( Matches = [] -> throw(named_midi_outlet_not_found(String))
   ; Matches = [ID-Name] -> format('% Connecting to MIDI outlet ~w.\n',[Name]), midi_connect(ID)
   ; throw(named_midi_outlet_not_unique(String,Matches))
   ).

%% midinote(+Ch:integer, +NN:integer, +Vel:integer, +Dur:nonneg) is det.
%
%  Play a MIDI note now.
midinote(Ch,NN,Vel,Dur) :-
	get_time(T), midinote_at(T,Ch,NN,Vel,Dur).

%% midinote_at(+T:time, +Ch:integer, +NN:integer, +Vel:integer, +Dur:nonneg) is det.
%
%  Schedule MIDI note at given time.
midinote_at(T,Ch,NN,Vel,Dur) :-
	midi_outlet(_,M), 
	midi(M,T,note(Ch,NN,Vel,Dur)).

