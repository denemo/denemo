;;;;;;;; PointAndClick
(if (d-Directive-score? "PointAndClick")
	(d-DirectiveDelete-score "PointAndClick")
	(d-DirectivePut-score-prefix "PointAndClick" "\n\\pointAndClickOff\n"))
(d-SetSaved #f)	
