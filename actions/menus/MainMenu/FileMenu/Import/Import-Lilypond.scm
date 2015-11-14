(if (d-GetSaved)
	(d-ImportLilypond)
	(d-WarningDialog (_ "You have unsaved work.")))