(let ((tag "BarNumbers") (params BarNumbers::params))
(if params
	(d-DirectivePut-score-postfix tag "\\context Score \\applyContext #(set-bar-number-visibility 4000)" )
	(ToggleDirective "score" "postfix" "BarNumbers" "\\context Score \\applyContext #(set-bar-number-visibility 4000)" )))