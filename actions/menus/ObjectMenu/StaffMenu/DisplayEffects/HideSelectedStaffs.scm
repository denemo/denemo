;;;HideSelectedStaffs
(let ((params HideSelectedStaffs::params))
    (if (d-MarkStatus)
        (begin
            (d-PushPosition)
            (while (d-MoveToStaffUp))
            (d-StaffHidden (d-IsInSelection))
            (while (d-MoveToStaffDown)
                (d-StaffHidden (d-IsInSelection)))
            (d-PopPosition)
            (d-StaffHidden #t))))
