;;;;;;;;;;CompareScores
(let ((staffnum #f)(message #f)(first #f)(second #f)(movement 1)(nummv1 1)(nummv2 1)(diff 0)(num #f))
    (d-Open)
    (set! nummv1 (d-GetMovementsInScore))
    (d-GoToPosition movement 1 1 1)
    (set! first (d-SelectTab))
    (d-NewWindow)
    (d-Open)
    (set! nummv2 (d-GetMovementsInScore))
    (d-GoToPosition movement 1 1 1)
    (set! second (d-SelectTab))        
    (set! staffnum (1- (d-GetStaff)))
    (set! diff (- nummv1 nummv2))
    (if (not (zero? diff))
        (d-WarningDialog (_ "Scores have different numbers of movements")))
    ;check score headers
    (set! message (d-DifferenceOfProjects first second))
    (if message
       (d-WarningDialog  (string-append (_ "Scores have different header information: ") message)))
    (set! num (if (positive? diff) nummv2 nummv1))
    (let loop ((counter 1)) ;; for each movement
        (d-SelectTab first)
        (d-GoToPosition counter 1 1 1)
        (d-SelectTab second)
        (d-GoToPosition counter 1 1 1)
        
        ;check movement headers
        (set! message (d-DifferenceOfMovements first second))
        (if message
            (d-WarningDialog (string-append (_ "Movement number ") (number->string (d-GetMovement))(_ "Movements have different header information: "))))

        (CheckTabs first second)
        (if (< counter num)
            (begin
                (d-WarningDialog (string-append (_ "Finished Movemnt ") (number->string counter)))
                (loop (1+ counter))))))
 (d-InfoDialog (_ "Finished, but lyric verses are not checked"))
