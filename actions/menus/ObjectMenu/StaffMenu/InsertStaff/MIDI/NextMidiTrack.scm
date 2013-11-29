;;;NextMidiTrack
(let ((max (d-GetImportedMidiTracks)))
	(if max
			(if (< (d-GetCurrentMidiTrack) max)
				(d-LoadMidiTrack (number->string (1+ (d-GetCurrentMidiTrack))))
				(d-WarningDialog (_ "This is the last track")))
			(d-WarningDialog (_ "No MIDI file loaded"))))
(d-RefreshDisplay)


