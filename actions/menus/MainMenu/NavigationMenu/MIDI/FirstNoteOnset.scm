;;;FirstNoteOnset
(let ((max (d-GetImportedMidiTracks)))
	(if max
		(d-AdvanceMarkedMidi 0)
		(d-WarningDialog (_ "No MIDI file has been loaded"))))
(d-RefreshDisplay)