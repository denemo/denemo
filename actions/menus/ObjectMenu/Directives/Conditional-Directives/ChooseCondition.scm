;;;ChooseCondition
(if (d-Directive-standalone?)
	(SetDirectiveConditional ChooseCondition::params)
	(d-WarningDialog (_ "Cursor not on a standalone directive - use Edit menu")))
