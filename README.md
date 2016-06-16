# plmidi
Access to Mac OS X MIDI input and output for SWI Prolog

This module allows Prolog code to send and receive MIDI events using
the CoreMIDI framework on Mac OS X.



## PREREQUISITES

- SWI Prolog
- Mac OS X


## INSTALLATION

The build process installs a Prolog module file and a foreign library
to ~/lib/prolog by default. If you wish to change this, edit the root Makefile
accordingly and be sure that the referenced directories are in your
Prolog file_search_path.

In the root directory of this package, type

	$ make install



## USAGE

This is the sort of thing that might work for you. (It is up to
you to manage the connection references returned by midi_mk_outlet/2.)

	:- use_module(library(plmidi)).
	:- dynamic midi_outlet/1.

	init :-
		midi_listall,
		midi_mk_outlet(1,Ref),
		assert(midi_outlet(Ref)).

	% play middle C in 1 second for 2 seconds
	bing :-
		midi_outlet(Ref),
		get_time(T),
		midi(Ref,T+1,note(0,60,100,2)).

	:- init.



## CHANGE LOG

v0.1 - 	initial release.
v0.2 -	got rid of some compiled warnings and removed messages about 
			timebase calibration.
v0.3 -   Merged in stuff from midi.pl to establish 'current' MIDI outlet.
v0.4 -  Made into a SWI Prolog pack
