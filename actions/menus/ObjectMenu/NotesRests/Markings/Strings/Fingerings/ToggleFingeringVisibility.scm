(if (ToggleHidden "note" "Fingering")
	(d-SetSaved #f)
	(d-InfoDialog (_ "No fingering on note at cursor")))