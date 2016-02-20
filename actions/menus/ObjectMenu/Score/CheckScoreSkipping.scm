;;CheckScoreSkipping
(define-once CheckScoreSkipping::number 1)
(define-once CheckScore::ignore 0)
(let ((saved (d-GetSaved))(skip (d-GetUserInput (_ "Check Score Skipping Some Errors") (_ "Give number of errors to skip") (number->string CheckScoreSkipping::number))))
    (d-IncreaseGuard)
    (if skip
        (begin
            (set! skip (string->number skip))
                (if skip
                    (begin
                        (set! CheckScoreSkipping::number skip)
                        (set! CheckScore::ignore skip)
                        (d-CheckScore #f) ;;;interactive call
                        
                        (if CheckScore::return
                            (begin
                                (if CheckScore::error-position
                                    (apply d-GoToPosition CheckScore::error-position))
                                (d-InfoDialog (string-append (_ "Error number: ") (number->string (+ 1 skip)) ": " CheckScore::return)))
                            (begin
                                (set! CheckScore::error-position #f)
                                (d-InfoDialog  (_ "No problem detected in this score")))))))
        (d-WarningDialog (_ "Cancelled")))
    (d-DecreaseGuard)
    (d-SetSaved saved))
