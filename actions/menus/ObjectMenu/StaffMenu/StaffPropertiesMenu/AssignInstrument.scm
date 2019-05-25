;;;AssignInstrument
(let ((num-staffs (d-GetStaffsInMovement)) (staff (d-GetStaff)))
	(SelectStaff)
	(d-Copy)
	(d-AddInstrumentStaff)
	(if (> (d-GetStaffsInMovement) (1+ num-staffs))
		(d-WarningDialog (_ "Copy and Paste the music from the old staffs to the new, and then delete the old staffs"))
		(if (>= (d-GetStaff) (1+ staff))
			(begin
				(while (> (d-GetStaff) (1+ staff))  (d-SwapStaffs))
				(d-Paste)
				(d-MoveToStaffUp)
				(d-DeleteStaff))
			(d-WarningDialog (_ "Cancelled")))))
	
