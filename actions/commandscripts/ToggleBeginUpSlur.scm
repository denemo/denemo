;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;ToggleNoteUpSlur
(let ((tag "Slur"))
	(if (d-IsSlurStart)
		(d-ToggleBeginSlur)
		(begin
			(d-ToggleBeginSlur)
			(d-DirectivePut-chord-postfix tag "^")
			
			;;for some reason we do not have ⏝ in the font
			(d-DirectivePut-note-graphic tag "\n^
			Denemo
			30")))
	(d-RefreshDisplay)
	(d-SetSaved #f))
