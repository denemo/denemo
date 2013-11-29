;;;PreviousMidiTrack
(let ((max (d-GetImportedMidiTracks)))
	(if max
			(if (> (d-GetCurrentMidiTrack) 1)
				(d-LoadMidiTrack (number->string (1- (d-GetCurrentMidiTrack))))
				(d-WarningDialog (_ "This is the first track")))
			(d-WarningDialog (_ "No MIDI file loaded"))))
(d-RefreshDisplay)
