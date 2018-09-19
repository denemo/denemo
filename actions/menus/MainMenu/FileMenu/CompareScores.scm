;;;;;;;;;;CompareScores
(let ((staffnum #f)(message #f)(first #f)(second #f))
    (d-Open)
    (d-GoToPosition 1 1 1 1)
    (set! first (d-SelectTab))
    (d-NewWindow)
    (d-Open)
    (d-GoToPosition 1 1 1 1)
    (set! second (d-SelectTab))        
    (set! staffnum (1- (d-GetStaff)))
;check score headers
    (set! message (d-DifferenceOfProjects first second))
    (if message
       (d-WarningDialog  (string-append (_ "Scores have different header information: ") message)))
;check movement headers
    (set! message (d-DifferenceOfMovements first second))
    (if message
            (d-WarningDialog (string-append (_ "Movement number ") (number->string (d-GetMovement))(_ "Movements have different header information: "))))
;check staff headers    
    (while (= (d-GetStaff) (1+ staffnum))
        (set! staffnum (1+ staffnum))
        (set! message (d-DifferenceOfStaffs first second)) ;moves to staff below after creating difference string
        (if message
             (d-WarningDialog (string-append (_ "Staff number ") (number->string (d-GetStaff)) ": " (_ "Staffs have different properties: ") message))))
;check staff contents
    (let loop ((staff 1))
        (d-SelectTab first)(disp "Doing staff " staff "\n")
        (if (d-GoToPosition 1 staff 1 1)
            (let ((move #f))
                (d-SelectTab second)
                (if (d-GoToPosition 1 staff 1 1)
                    (let inner-loop ()
                        (set! message (d-CompareObjects first second move))
                        (if (not message)
                                (loop (+ 1 staff))
                                (let ((response
                                        (d-GetUserInput (_ "Comparing Music") (string-append message "\n" (_ "Continue Searching?")) "y")))
                                     (if (and (string? response) (equal? response "y"))
                                        (begin
                                            (set! move #t)
                                            (inner-loop))
                                        (loop (+ 1 staff))))))
                      (d-InfoDialog (_ "Different numbers of staffs."))))))
    (d-InfoDialog (_ "No (further) differences found, but lyric verses are not checked, and additional staffs in the second score will be ignored")))
