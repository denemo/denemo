;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;ToggleNoteDownTie
(let ((tag "Tie"))
	(if (d-Directive-note? tag)
		(d-DirectiveDelete-note tag)
		(begin
			(d-DirectivePut-note-postfix tag "-\\tweak #'direction #-1 ~")
			;;for some reason we do not have ⏝ in the font
			(d-DirectivePut-note-graphic tag "\n~
			Denemo
			30")))
	(d-RefreshDisplay)
	(d-SetSaved #f))
