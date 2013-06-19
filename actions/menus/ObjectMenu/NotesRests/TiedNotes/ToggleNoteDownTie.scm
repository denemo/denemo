;;ToggleNoteDownTie
(let ((tag "Tie"))
	(if (d-Directive-note? tag)
		(d-DirectiveDelete-note tag)
		(begin
			(d-DirectivePut-note-postfix tag "_~ ")
			(d-Chordize #t)
			;;for some reason we do not have ⏝ in the font
			(d-DirectivePut-note-graphic tag "\n~
			Denemo
			30")))
	(d-RefreshDisplay)
	(d-SetSaved #f))
