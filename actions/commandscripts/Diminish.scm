;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(SingleAndSelectionSwitcher  (lambda ()
(if (duration::GetBaseDurationInTicks)
 (duration::ChangeNoteDurationInTicks (/ (duration::GetBaseDurationInTicks) 2) (d-GetDots)))))
	