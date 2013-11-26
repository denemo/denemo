;;;GuidedMidiImport
(if  (d-New)
(begin
	(d-MasterVolume 0)
	(d-ImportMidi "guided=true")
	(d-CreateClickStaffForMidi)
	(d-LoadMidiTrack)
	(d-MasterVolume )))
