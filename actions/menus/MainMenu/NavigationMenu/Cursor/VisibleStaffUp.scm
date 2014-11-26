;;;VisibleStaffUp
(while (and (d-MoveToStaffUp) (d-StaffHidden)))
(if (d-StaffHidden)
	(while (and (d-MoveToStaffDown) (d-StaffHidden))))
	
	