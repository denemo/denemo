;;;CheckTimeSignatures
 (define-once CheckScore::ignore 0)
 (define CheckTimeSignatures::return #f)
 (define-once CheckScore::error-position #f)
 (let ()
    (define position (GetPosition))
    (define (check-down timesig)
        (define result #f)
        (d-PushPosition)
        (if (d-MoveToStaffDown) 
           (begin
             (while (d-NextObjectInMeasure))
             (if (not (equal? timesig (d-GetPrevailingTimesig)))
                (if (positive? CheckScore::ignore)
                    (set! CheckScore::ignore (1- CheckScore::ignore))
                    (begin
                        (set! result (string-append  (_ "Time Signature does not match ") timesig " : " (d-GetPrevailingTimesig)))
                        (set! CheckScore::error-position (GetPosition)))))))
        (d-PopPosition)
        result)
        
     (while (d-MoveToStaffUp))
     (let outer-loop ((first #t))
         (d-MoveToBeginning)
         (let measure ()
                    (define result #f)
                    (while (and (not result) (d-NextObjectInMeasure))
                            (if (and (Timesignature?) (not (zero? (d-GetStartTick))))
                                    (if (positive? CheckScore::ignore)
                                        (set! CheckScore::ignore (1- CheckScore::ignore))
                                    (begin
                                        (set! result #t)
                                        (set! CheckScore::error-position (GetPosition))
                                        (set! CheckTimeSignatures::return (_ "Time Signature not at start of measure"))))))
                                    
                    (if (not result)
                        (let ((timesig (d-GetPrevailingTimesig)))
                                (begin
                                    (set! result (check-down timesig))
                                    (if result
                                        (set! CheckTimeSignatures::return result)
                                        (if (d-MoveToMeasureRight)
                                            (measure)))))))
        (if (and (not CheckTimeSignatures::return) (d-MoveToStaffDown))
            (outer-loop #f)))
    (if (not CheckTimeSignatures::params) ;;; interactive when #f
        (begin
            (if CheckTimeSignatures::return
                (begin
                    (apply d-GoToPosition CheckScore::error-position)
                    (d-WarningDialog CheckTimeSignatures::return))
                (d-InfoDialog (_ "No problem detected with time signature changes"))))))
