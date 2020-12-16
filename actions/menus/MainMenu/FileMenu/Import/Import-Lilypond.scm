(if (d-GetSaved)
	(begin
		(d-New)
		(d-ImportLilypond))
	(d-WarningDialog (_ "You have unsaved work.")))