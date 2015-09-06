;;;TimesigWithBeatStructure
(let ((tag "TimesigWithBeatStructure") (beat #f) (divisions #f) (time #f)(DefaultbaseMomentFraction #f))
    (define (timesig val)
            (let* ((index (string-index val #\/))
                    (numerator (string->number (substring val 0 index)))
                    (denominator (string->number (substring val (+ index 1)))))
                    (set! DefaultbaseMomentFraction (string-append "1/" (number->string denominator)))

                  (cons numerator denominator)))
   (if (not (d-Directive-timesig? tag))
    (begin
        (d-InsertTimeSig)
        (d-MoveCursorLeft)))
        
    (set! time (timesig (d-GetPrevailingTimesig)))
    (set! beat (d-GetUserInput (_ "Beat Structure") (_ "Give beat:\n(smallest note that beams can split at\ne.g. 1/8 for eighth notes.)") DefaultbaseMomentFraction))
    
    (if (and beat (string->number beat))
        (begin
            (set! divisions (* (car time) (/ (/ 1 (cdr time)) (string->number beat))))
            (set! divisions (d-GetUserInput (_ "Beat Structure")
                                            (_ "Give grouping of beats desired:\nA set of numbers with spaces between each number is how many beats before the beam breaks.")
                                            (DenemoDefaultBeatStructure divisions)))
            (if divisions
                (begin
                    (d-RefreshDisplay)
                    (d-SetSaved #f)
                    (d-DirectivePut-timesig-prefix tag (string-append "\\overrideTimeSignatureSettings " (d-GetPrevailingTimesig) " " beat " #'(" divisions ") #'()"))
                    (d-DirectivePut-timesig-gy tag -10)
                    (d-DirectivePut-timesig-graphic tag (string-append "\n" divisions "\nDenemo\n12")))))))
