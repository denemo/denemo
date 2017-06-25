;;;ToggleFingeringVisibilityInScore
(let ((tag "ToggleFingeringVisibilityInScore"))
	(if (d-Directive-score? tag)
		(begin
			(d-DirectiveDelete-score tag)
			(d-InfoDialog (_ "Fingerings will be typeset")))
		(begin
			(d-DirectivePut-score-prefix tag "\n\\layout { \\omit Fingering }\n")
			(d-InfoDialog (_ "Fingerings will be not typeset")))))
(d-SetSaved #f)
