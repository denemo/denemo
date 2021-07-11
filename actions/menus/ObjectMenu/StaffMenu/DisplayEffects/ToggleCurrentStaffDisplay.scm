;;;ToggleCurrentStaffDisplay
(d-StaffHidden (not (d-StaffHidden)))
(while (and  (d-StaffHidden) (d-MoveToStaffDown)))
(while (and  (d-StaffHidden) (d-MoveToStaffUp)))
(if (d-Directive-clef? DenemoClickTrack)
	(begin
		(d-MoveToStaffDown)
		(d-StaffHidden #f)))
(d-RefreshDisplay)
(d-SetSaved #f)