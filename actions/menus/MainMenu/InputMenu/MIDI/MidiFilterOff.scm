;;;MidiFilterOff
(d-PutMidi 0);;; Our own Reset, this makes the filter toggle off/on
(d-InputFilterNames (_ "No MIDI filter active"))
	(d-PlayMidiKey #x204001)
	(d-PlayMidiKey #x202201)
