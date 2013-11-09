;InsertTiedNoteDuration 
; Toggles a tie and moves right for insert with duration key. Gives audible feedback
(d-ToggleTie)
(if (d-IsTied)
	(begin
		(d-PlayMidiNote 59 255 9 10)
		(d-PendingMidi 71))
	(d-PlayMidiNote 41 255 9 10))
(if (not (d-IsAppending))
	(d-MoveCursorRight))