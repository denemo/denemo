;InsertTiedNoteDuration  Insert tuplet into the score
(d-ToggleTie)
(if (d-IsTied)
	(d-PendingMidi 75)
	(d-PlayMidiNote 35 255 9 100))
(if (not (d-IsAppending))
	(d-MoveCursorRight))