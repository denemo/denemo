;;;GuidedMidiImport
(if  (d-New)
(begin
	(d-ImportMidi "guided=true")
	(d-CreateClickStaffForMidi)
	(d-LoadMidiTrack)))
