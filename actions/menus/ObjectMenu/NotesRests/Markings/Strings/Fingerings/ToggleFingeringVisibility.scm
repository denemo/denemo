;;;ToggleFingeringVisibility
(let ((params ToggleFingeringVisibility::params))
	(if (ToggleHidden "note" "Fingering")
		(d-SetSaved #f)
		(if (not params)
			(d-InfoDialog (_ "No fingering on note at cursor")))))