;;;;NewScoreFromCurrent
(let ((warning #f))
    (define (do-book field)
        (if (d-Directive-scoreheader?  field)
                    (eval-string (string-append "(d-" field ")"))))
                    
    (define (do-simple-titles tag data)
                    (define (do-one field)
                        (if (assq-ref data field)
                                (DenemoSetTitles tag field #t)))
                    (if data 
                        (begin
                            (set! data (eval-string data))
                            
                            (do-one 'dedication)
                            (do-one 'title)
                            (do-one 'subtitle)
                            (do-one 'subsubtitle)
                            (do-one 'instrument)
                            (do-one 'poet)
                            (do-one 'composer)
                            (do-one 'meter)
                            (do-one 'arranger)
                            (do-one 'tagline)
                            (do-one 'copyright)
                            (do-one 'piece)
                            (do-one 'opus))))
    (define (refresh-mirror-staff)
        (let ((staff-num (d-DirectiveGet-voice-data "SubstituteMusic")))
            (if staff-num
                (d-SubstituteMusic (string->number staff-num)))))

    (d-GoToPosition 1 1 1 1)
    (if (d-GetSaved)
           (begin
                (if (d-Directive-standalone? "DenemoLink")
                    (begin
                        (d-SetMark)
                        (d-Copy))
                     (d-ClearClipboard))
                (d-DirectiveDelete-scoreheader "ScoreIncipit")
                (d-DeleteFromCursorToEnd 'all)
                (let loop () 
                    (if (d-NextMovement)
                        (begin 
                            (d-DeleteMovement)
                            (d-GoToPosition 1 1 1 1) 
                             (loop))))
                (d-Paste)
                (let ((key (d-GetPrevailingKeysigName)))
                    (set! key (d-GetUserInput (_ "Create Template") (_ "Give key") key))
                    (if key
                        (begin
                            (d-InitialKey key)
                            (while (d-StaffDown)
                                (d-InitialKey key))
                            (while (d-StaffUp)))))
                            
                (let ((time (d-GetPrevailingTimesig)))
                    (set! time (d-GetUserInput (_ "Create Template") (_ "Give time signature") time))
                    (if time
                        (begin
                            (d-InitialTimeSig time)
                            (while (d-StaffDown)
                                (d-InitialTimeSig time))
                            (while (d-StaffUp)))))
                            
                (do-book "BookTitle")
                (do-book "BookSubtitle")
                (do-book "BookSubsubtitle")
                (do-book "BookComposer")
                (do-book "BookMeter")
                (do-book "BookArranger")
                (do-book "BookDate")
                (do-book "BookPoet")
                (do-book "BookCopyright")
                (do-book "BookDedication")
                (do-book "BookPiece")
                (do-book "BookOpus")
                (do-book "BookTagline")
                (do-simple-titles "ScoreTitles" (d-DirectiveGet-scoreheader-data "ScoreTitles"))
                (do-simple-titles "MovementTitles" (d-DirectiveGet-header-data "MovementTitles"))
                (refresh-mirror-staff)
                (while (d-StaffDown)
                    (refresh-mirror-staff))
                )
            (set! warning (string-append (_ "Cancelled: ") (_ "Score is not saved"))))
    (if warning
        (d-WarningDialog warning)
        (d-ClearFilename)))

