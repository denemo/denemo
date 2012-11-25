;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;InsertTiedNoteDuration 
; Toggles a tie and moves right for insert with duration key. Gives audible feedback
(d-ToggleTie)
(if (d-IsTied)
	(d-PendingMidi 75)
	(d-PlayMidiNote 35 255 9 100))
(if (not (d-IsAppending))
	(d-MoveCursorRight))