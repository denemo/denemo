(SingleAndSelectionSwitcher  (lambda ()
(if (duration::GetBaseDurationInTicks)
 (duration::ChangeNoteDurationInTicks (* (duration::GetBaseDurationInTicks) 2) (d-GetDots)))))
		