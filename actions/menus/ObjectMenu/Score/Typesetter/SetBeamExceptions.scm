;;;SetBeamExceptions
(let ((tag "SetBeamExceptions")
    (exceptions #f)(data #f)
        (timeSignatureFraction #f) 
        (beatStructure #f) 
        (baseMomentFraction #f)
        (DefaulttimeSignatureFraction #f) 
        (DefaultbeatStructure #f) 
        (DefaultbaseMomentFraction #f))
    (define (set-exceptions)
            (d-SetSaved #f)
            (d-DirectivePut-score-data tag (format #f "'~s" data))
            (let ((prefix ""))
                (define (do-append alist)
                    (let ((time (car alist))
                           (value (cdr alist)))
                    (define notes (assoc-ref value 'notes))
                    (define baseMomentFraction (assoc-ref value 'baseMomentFraction))
                    (define beatStructure (assoc-ref value 'beatStructure))
                    (define timeSignatureFraction (assoc-ref value 'timeSignatureFraction))
                    (if (> (string-length notes) 0)
                        (set! notes (string-append "\\beamExceptions {" notes "}\n"))
                        (set! notes "#'()\n"))
                    
                    (set! prefix (string-append prefix "\\layout {
    \\overrideTimeSignatureSettings "
                            timeSignatureFraction
                            " " baseMomentFraction
                             " #'("  beatStructure  ") " notes "}\n"))))
                (for-each do-append (eval-string (d-DirectiveGet-score-data tag)))
                (d-DirectivePut-score-prefix tag prefix)))                       

    (define (timesig val)
            (let* ((index (string-index val #\/))
                    (numerator (string->number (substring val 0 index)))
                    (denominator (string->number (substring val (+ index 1)))))
                  (set! DefaulttimeSignatureFraction val)
                  (set! DefaultbaseMomentFraction (string-append "1/" (substring val (+ index 1))))
                  (set! DefaultbeatStructure (DenemoDefaultBeatStructure numerator))
                  (cons numerator denominator)))

   
     (define (get-lilypond)
                (define lily (d-GetLilyPond))
                (if lily
                    (let ((position (string-contains lily "\\noBeam")))
                        (if position
                                (string-append (substring lily 0 position) "[]")
                                lily))
                    #f))
     (define (get-exceptions)
           (if (MoveToSelectionBeginningInThisStaff)
                (let ((time #f)(measure (d-GetMeasure)))
                    (set! time (format #f "~s" (timesig (d-GetPrevailingTimesig))))
                    (set! exceptions (get-lilypond))
                        (if exceptions
                            (begin
                                (let loop ((num (d-GetMeasure)))
                                    (if (d-NextSelectedObject)
                                        (begin
                                              (if (not (= num (d-GetMeasure)))
                                                (set! exceptions (string-append exceptions " | ")))
                                             (set! exceptions (string-append exceptions (get-lilypond)))
                                             (loop (d-GetMeasure)))))
                                (set! exceptions (cons time exceptions)))))))
                        
;;;;;;;;;routine starts here                        
    (set! data (d-DirectiveGet-score-data tag))
    (if data
            (set! data (eval-string data))
            (set! data '()))
    (get-exceptions) 
    (if exceptions
        (let ((value (assoc-ref data (car exceptions))))
                (if value
                    (let ((choice (RadioBoxMenu (cons (_ "Add to current rules") 'add)
                                                (cons (_ "Replace current rules") 'replace)
                                                (cons (_ "Delete (and revert to default rules)") 'delete))))
                                                
                        (set! baseMomentFraction (assoc-ref value 'baseMomentFraction))
                        (if (not baseMomentFraction)
                             (set! value (assoc-set! value 'baseMomentFraction DefaultbaseMomentFraction)))
                        (if (not beatStructure)
                             (set! value (assoc-set! value 'beatStructure DefaultbeatStructure)))
                               
                        (set! timeSignatureFraction (assoc-ref value 'timeSignatureFraction))
                        (if (not timeSignatureFraction)
                             (set! value (assoc-set! value 'timeSignatureFraction DefaulttimeSignatureFraction)))
                             
                        (case choice
                            ((add)
                                (let ((this (assoc-ref value 'notes)))
                                        (if this
                                                (set! this (string-append this " | "(cdr exceptions)))
                                                (set! this (cdr exceptions)))
                                                
                                        (set! value (assoc-set! value 'notes this)))
                                        (set! data (assoc-set! data (car exceptions) value))   
                                        (set-exceptions))
                            ((replace)
                            
                                        (set! value (assoc-set! value 'notes (cdr exceptions)))
                                        (set! data (assoc-set! data (car exceptions) value))  
                                        (set-exceptions))
                            ((delete)
                                (d-SetSaved #f)
                                (set! data (assoc-remove! data (car exceptions)))
                                (set-exceptions))
                            (else
                                (d-InfoDialog (_ "Cancelled")))))
                    (begin
                        (if (not baseMomentFraction)
                             (set! baseMomentFraction DefaultbaseMomentFraction))
                        (if (not beatStructure)
                             (set! beatStructure DefaultbeatStructure))
                        (if (not timeSignatureFraction)
                             (set! timeSignatureFraction DefaulttimeSignatureFraction))
                        (set! value (assoc-set! value 'notes (cdr exceptions)))
                        (set! value (assoc-set! value 'baseMomentFraction baseMomentFraction))
                        (set! value (assoc-set! value 'beatStructure beatStructure))
                        (set! value (assoc-set! value 'timeSignatureFraction timeSignatureFraction))
                        (set! data (assoc-set! data (car exceptions) value))   
                        (set-exceptions))))
           (let*    ((time  (timesig (d-GetPrevailingTimesig)))
                    (choice (RadioBoxMenu (cons (string-append (_ "Set Beaming rule for ") DefaulttimeSignatureFraction)  'set)
                                                (cons (string-append (_ "Delete Beaming rule for ") DefaulttimeSignatureFraction)  'delete)
                                                (cons (_ "Delete All (and revert to default rules)") 'deleteAll))))
                                                
              (case choice
                            ((set) 
                                (let ((value '()) (numbeats #f))
                                    (let loop ()
                                        (set! baseMomentFraction (DenemoGetDuration (_ "Give beat")))
                                        (if baseMomentFraction
                                            (begin
                                                (set! numbeats (* (car time) (/ (/ 1 (cdr time)) (string->number baseMomentFraction))))
                                                (if (not (integer? numbeats))
                                                    (begin
                                                        (d-WarningDialog (_ "Chosen duration does not fit time signature"))
                                                        (loop))))))
                                    (if baseMomentFraction
                                        (begin
                                            (set! beatStructure (d-GetUserInput (_ "Beat Structure") (string-append (_ "Give groupings for ") (number->string numbeats) (_ " beats: ")) (DenemoDefaultBeatStructure numbeats)))
                                            (set! value (assoc-set! value 'baseMomentFraction baseMomentFraction))
                                            (set! value (assoc-set! value 'beatStructure beatStructure))
                                            (set! value (assoc-set! value 'timeSignatureFraction DefaulttimeSignatureFraction))
                                            (set! value (assoc-set! value 'notes ""))
                                            (set! data (assoc-set! data (format #f "~s" time) value))  
                                            (set-exceptions))
                                        (d-InfoDialog (_ "Cancelled")))))
                             ((delete)
                                (set! data (assoc-remove! data (format #f "~s" time)))
                                (set-exceptions))
                        
                            ((deleteAll)
                                (d-SetSaved #f)
                                (d-DirectiveDelete-score tag))
                            (else
                                (d-WarningDialog (_ "Cancelled")))))))
                                
