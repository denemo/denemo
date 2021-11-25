;;MuteStaffs
(let ((tag "MuteStaffs") (mute "MuteStaff") (numstaffs (d-GetStaffsInMovement)) (thelist '()))
    (d-PushPosition)
    (d-MoveToMovementBeginning)
    (let loop ()
        (if (not (d-Directive-standalone? mute))
                (d-MuteStaff)) ;create Mute directive and make unmuted
        (set! thelist (cons (cons (d-StaffProperties "query=denemo_name") (<= (d-StaffMasterVolume) 0)) thelist))
        (if (d-MoveToStaffDown)
            (loop)))
 (set! thelist (reverse thelist))
 (set! thelist (d-CheckBoxes thelist (_ "Choose Staffs to Mute")))
 (if thelist
    (begin
        (d-MoveToMovementBeginning)
        (let loop ()
            (if (not (eq? (cdar thelist) (<= (d-StaffMasterVolume) 0)))
                (d-MuteStaff))
            (set! thelist (cdr thelist))
            (if (d-MoveToStaffDown)
                (loop))))
    (d-WarningDialog (_ "Cancelled")))
    (d-PopPosition))
        
