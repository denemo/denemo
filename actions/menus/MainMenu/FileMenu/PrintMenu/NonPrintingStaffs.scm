;;;NonPrintingStaffs
(let ((tag "NonPrintingStaff")  (numstaffs (d-GetStaffsInMovement)) (thelist '()))
    (d-PushPosition)
    (d-MoveToMovementBeginning)
    (let loop ()
        (set! thelist (cons (cons (d-StaffProperties "query=denemo_name") (not (d-DirectiveGetForTag-staff tag))) thelist))
        (if (d-MoveToStaffDown)
            (loop)))
 (set! thelist (reverse thelist)) 
 (set! thelist (d-CheckBoxes thelist (_ "Choose Staffs to Print")))
 (if thelist
    (begin
        (d-MoveToMovementBeginning) 
        (let loop () ;(disp (cdar thelist))
            (if  (cdar thelist)
                (d-NonPrintingStaff 'unset)
                (d-NonPrintingStaff 'set))
            (set! thelist (cdr thelist)) 
            (if (d-MoveToStaffDown)
                (loop))))
    (d-WarningDialog (_ "Cancelled"))))
  