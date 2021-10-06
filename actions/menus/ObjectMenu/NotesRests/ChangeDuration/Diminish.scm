(SingleAndSelectionSwitcher  (lambda ()
	(if (d-Directive-chord? "WholeMeasureRest")
		(d-WholeMeasureRest)
		(if (duration::GetBaseDurationInTicks)
			(duration::ChangeNoteDurationInTicks (/ (duration::GetBaseDurationInTicks) 2) (d-GetDots))))))
	
