;;;ToggleCurrentStaffDisplay
(d-StaffHidden (not (d-StaffHidden)))
(while (and  (d-StaffHidden) (d-MoveToStaffDown)))
(while (and  (d-StaffHidden) (d-MoveToStaffUp)))
(d-RefreshDisplay)
(d-SetSaved #f)