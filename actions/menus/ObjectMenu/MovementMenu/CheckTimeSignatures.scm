;;;CheckTimeSignatures
 (define CheckTimeSignatures::return #f)
 (let ()
    (define position (GetPosition))
    (define (check-down timesig)
        (define result #f)
        (d-PushPosition)
        (while (d-MoveToStaffUp))
        (let loop ()
            (if (not (and (Timesignature?) (equal? timesig (d-GetPrevailingTimesig))))
                (set! result (_ "Time Signature does not match"))
                (if  (d-MoveToStaffDown)
                	(loop))))
        (d-PopPosition)
        result)
        
     (while (d-MoveToStaffUp))
     (let outer-loop ()
         (d-MoveToBeginning)
         (let loop ()
                    (define result #f)
                    (if (Timesignature?)
                        (let ((timesig (d-GetPrevailingTimesig)))
                            (if (d-PrevObjectInMeasure)
                                (begin
                                    (set! CheckTimeSignatures::return (_ "Time Signature not at start of measure")))
                                (begin
                                    (set! result (check-down timesig))
                                    (if result
                                        (set! CheckTimeSignatures::return result)
                                        (if (d-MoveCursorRight)
                                            (loop))))))
                        (if (d-MoveCursorRight)
                            (loop))))
        (if (d-MoveToStaffDown)
            (outer-loop)))
    (if CheckTimeSignatures::params
        (apply d-GoToPosition position)
        (begin
            (if CheckTimeSignatures::return
                (d-WarningDialog CheckTimeSignatures::return)
                (d-InfoDialog (_ "No problem detected with time signature changes"))))))
                            