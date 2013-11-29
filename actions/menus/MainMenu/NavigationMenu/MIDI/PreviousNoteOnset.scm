;;;PrevioustNoteOnset
(let ((max (d-GetImportedMidiTracks)))
	(if max
		(begin
			(if (d-GetMarkedMidiNote)			
				(d-AdvanceMarkedMidi -1)
				(d-WarningDialog (_ "No Onset has been marked"))))
		(d-WarningDialog (_ "No MIDI file has been loaded"))))
(d-RefreshDisplay)