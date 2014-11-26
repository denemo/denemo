;;;HideNotSelectedStaffs
(let ((params HideNotSelectedStaffs::params))
    (if (d-MarkStatus)
        (begin
            (d-PushPosition)
            (while (d-MoveToStaffUp))
            (d-StaffHidden (not (d-IsInSelection)))
            (while (d-MoveToStaffDown)
                (d-StaffHidden (not (d-IsInSelection))))
            (d-PopPosition)
            (d-StaffHidden #f))))
        
            
