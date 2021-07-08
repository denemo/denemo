;;Backup and Play Marked Midi
(d-AdvanceMarkedMidi -1)
(if (not (d-GetMarkedMidiNote))
	(d-SetMarkedMidiNote -1))
(d-PlayMarkedMidi)