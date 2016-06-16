# plmidi
Access to Mac OS X MIDI input and output for SWI Prolog

This module allows Prolog code to send and receive MIDI events using
the CoreMIDI framework on Mac OS X.


## USAGE

	:- use_module(library(plmidi)).

	bing :-
		midi_endpoint(outlet(Id, Name, _, _)),
		format('Connecting to MIDI endpoint "~w".\n',[Name]),
		midi_mk_outlet(Id,Ref),

		% play middle C in 1 second for 2 seconds
		get_time(T),
		midi(Ref,T+1,note(0,60,70,2)).


