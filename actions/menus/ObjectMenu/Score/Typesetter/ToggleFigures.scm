;;;ToggleFigures
(let ((tag "ToggleFigures")(params ToggleFigures::params))
	(if (d-Directive-score? tag)
		(begin
			(d-DirectiveDelete-score tag)
			(if (not params) (d-InfoDialog (_ "Bass Figures will be typeset"))))
		(begin
			(d-DirectivePut-score-prefix tag "\n\\layout {
  \\context { \\Staff
             \\omit BassFigure
           }\n}\n")
			(if (not params) (d-InfoDialog (_ "Bass Figures will not be typeset"))))))
(d-SetSaved #f)
