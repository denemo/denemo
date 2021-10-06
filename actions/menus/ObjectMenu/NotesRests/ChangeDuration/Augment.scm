(SingleAndSelectionSwitcher  (lambda ()
(if (duration::GetBaseDurationInTicks)
		(if (d-Directive-chord? "WholeMeasureRest")
			(d-WholeMeasureRest)
			(duration::ChangeNoteDurationInTicks (* (duration::GetBaseDurationInTicks) 2) (d-GetDots))))))
			
