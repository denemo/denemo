;;;TimeSignature
(let ((tag "TimeSignature")(numerator #f)(denominator #f))
    (d-InsertTimeSig)
    (d-MoveCursorLeft)
    (if (Timesignature?)
    (let ((scale (/ (string->number (d-ScoreProperties "query=fontsize")) 7.2)))
    	(define (scale-val val)
    		(number->string (* (string->number val) scale)))
        (set! numerator (car (string-split (d-GetPrevailingTimesig) #\/)))
        (set! denominator (cadr (string-split (d-GetPrevailingTimesig) #\/)))
        (d-DirectivePut-timesig-postfix tag (string-append "<>-\\tweak #'extra-offset #'("
        (scale-val "-4") " . " (scale-val "-4") ") -\\tweak baseline-skip #" (scale-val "2") "   "  "^\\markup\\scale #'(1.5 . 1.8)\\column{\\line\\large{\\bold "numerator "}\\line\\large{\\bold " denominator "}}"))
        (d-SetSaved #f)
        (d-RefreshDisplay))))  
