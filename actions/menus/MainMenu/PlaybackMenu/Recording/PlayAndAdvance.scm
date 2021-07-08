;;Play and Advance Marked Midi
(d-PlayMarkedMidi)
(d-AdvanceMarkedMidi 1)
(if (not (d-GetMarkedMidiNote))
	(d-SetMarkedMidiNote 1))