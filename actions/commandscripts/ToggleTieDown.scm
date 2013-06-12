;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;ToggleTieDown
(let ((tag "Tie"))
	(if (d-IsTied)
		(begin
			(d-DirectiveDelete-chord tag)
			(d-ToggleTie))
		(begin
			(d-ToggleTie)
			(d-DirectivePut-chord-postfix tag "_")
			(d-DirectivePut-chord-graphic tag "\n_
			Denemo
			30")))
	(d-RefreshDisplay)
	(d-SetSaved #f))
