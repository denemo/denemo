;;;UnmarkNoteOnset
(let ((max (d-GetImportedMidiTracks)))
	(if max
		(d-AdvanceMarkedMidi #f)
		(d-WarningDialog (_ "No MIDI file has been loaded"))))
(d-RefreshDisplay)		