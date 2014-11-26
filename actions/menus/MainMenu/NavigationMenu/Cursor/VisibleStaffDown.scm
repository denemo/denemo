;;;VisibleStaffDown
(while (and (d-MoveToStaffDown) (d-StaffHidden)))
(if (d-StaffHidden)
	(while (and (d-MoveToStaffUp) (d-StaffHidden))))
	
	