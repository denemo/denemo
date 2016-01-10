;;;PianoStaff
(let ((name #f) (del (and (None?)
    (equal? (d-StaffProperties "query=denemo_name") "Unnamed"))))
    (d-TakeSnapshot)
    (d-IncreaseGuard)
    (if del
        (set! del (RadioBoxMenu
                     (cons (_ "Replace Current Staff?")   'replace)
                     (cons (_ "Keep Current Staff") #f))))              
    (d-AddAfter)
    (d-StaffProperties (_ "Piano"))
    (d-PianoStaffStart)
    (d-PianoStaffName)
    (d-AddAfter)
    (d-InitialClef "Bass")
    (d-StaffProperties (_ "Piano"))
    (d-BraceEnd)
    (if del
        (begin
            (d-MoveToStaffUp)
            (d-MoveToStaffUp)
            (d-DeleteStaff)))
    (d-DecreaseGuard))
    

