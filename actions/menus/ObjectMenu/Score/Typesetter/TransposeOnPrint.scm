;;;TransposeOnPrint
(define-once Transpose::Interval #f)
(let ((transpose "Transpose")(cancel "Cancel") (instruction (_ "⬅Edit the interval and click Transpose button above")))
    (d-NewWindow)
    (if  (string? Transpose::Interval)
        (let ((thenotes (string-split Transpose::Interval #\space)))
            (d-Insert2)
            (d-PutNoteName (car thenotes))
            (d-Insert2)
            (d-PutNoteName (cadr thenotes)))
         (begin
            (d-C) 
            (d-E) 
            (d-Flatten))) ;;;c ees, arbitrary choice.
    
    (d-DirectivePut-standalone-graphic "Instruction" (string-append "\n" instruction "\nDenemo 24"))
    (d-MoveCursorLeft)(d-MoveCursorLeft)
    (CreateButton transpose (string-append " <span font_desc=\"36\" foreground=\"blue\">" (_ "Transpose")"</span>"))
    (d-SetDirectiveTagActionScript transpose 
        "(let ((interval #f))
            (d-MoveToBeginning)
            (set! interval (d-GetNote))
            (d-MoveCursorRight) 
            (set! interval (string-append interval \" \" (d-GetNote)))
            (d-SetSaved #t)
            (d-Close)
            (d-TransposeScorePrint interval))"
        )
        
    (CreateButton cancel (string-append " <span font_desc=\"36\" foreground=\"black\">" (_ "Cancel")"</span>"))
    (d-SetDirectiveTagActionScript cancel 
        " (d-SetSaved #t)
            (d-Close)"))