;;;AssignInstrument
(let ((num-staffs (d-GetStaffsInMovement))(has-lyrics (d-GetVerse)) (staff (d-GetStaff)))
	(SelectStaff)
	(d-Copy)
	(d-AddInstrumentStaff)
	(if (or has-lyrics (> (d-GetStaffsInMovement) (1+ num-staffs)))
		(d-WarningDialog (_ "Copy and Paste the music from the old staffs to the new, and then delete the old staffs"))
		(if (>= (d-GetStaff) (1+ staff))
			(begin
				(while (and (> (d-GetStaff) (1+ staff))  (not (d-SwapStaffs "non-interactive"))))
				(if (= (d-GetStaff) (1+ staff))
					(begin 
						(d-Paste)
						(d-MoveToStaffUp)
						(d-DeleteStaff))
				(d-WarningDialog (_ "Copy and Paste the music from the old staff to the new, and then delete the old staff"))))	
			(d-WarningDialog (_ "Cancelled")))))
	
