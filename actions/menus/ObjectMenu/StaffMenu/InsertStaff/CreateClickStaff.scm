;;;CreateClickStaff
(let ((old_highlight (d-HighlightCursor #f))
      (old_volume (d-MasterVolume)))
      
    (define (insert-duration str)
        (if (and str (string->number str) (not (string-suffix? "." str)))
            (eval-string (string-append "(d-" str ")"))
            (d-InfoDialog (_ "Fill in the upbeat in the click track before playing"))))
    (define (duplicate-measures)
        (let loop ()
            (if (and (FullDurationMeasure?) (d-MoveToMeasureRight) (EmptyMeasure?))
                    (begin
                        (d-MoveToMeasureLeft)
                        (d-AddDuplicateMeasure)
                        (GoToMeasureBeginning)
                        (if (not (Music?))
                            (d-DeleteObject))
                        (loop))))
        (Timesignature?))
                
    (define (play-clicks-and-recurse)
         ;;;play this measure in the click staff
            (d-StaffToPlay 1)
            (d-SetPlaybackInterval (CurrentMeasureOnTime) (CurrentMeasureOffTime))
            (d-Play "(d-StaffToPlay)(d-CreateClickStaff)"))
            
;;;procedure adds beats to the bar and plays it the recursively call the procedure to ask if it should be duplicated.               
    (define (populate-measure)
    	(d-DirectiveDelete-standalone "Blank")
        (let* ((numerator  (duration::GetNumerator))
                (denominator (duration::lilypond->denemo (duration::GetDenominator)))
                (put-beat (eval-string (string-append "d-" (number->string denominator)))))
                (put-beat)
                (d-CursorUp) (d-CursorUp)
                (d-AddNoteToChord)
                (d-CursorDown) (d-CursorDown)
                (if (> numerator 1)
                    (let loop ((n (- numerator 2)))
                        (put-beat)
                        (if (positive? n)
                            (loop (1- n)))))
                (play-clicks-and-recurse)))

               
    ;;main procedure     
    (if (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name"))
            (begin
                (if (ZeroDurationMeasure?)
                    (populate-measure)
                    (if (FullDurationMeasure?)
                        (let ((choice (RadioBoxMenu (cons (_ "Populate with these clicks") 'populate) (cons (_ "Play clicks again") 'play) (cons (_ "Edit clicks") 'edit))))
                            (case choice
                                ((populate)
                                    (if (duplicate-measures)
                                        (begin
                                            (d-MoveCursorRight)
                                            (populate-measure))))   
                                ((play)
                                    (play-clicks-and-recurse))
                                ((edit)
                                    (d-InfoDialog (_ "Edit the clicks and then re-run this command to populate the subsequent empty measures up to any time signature change. Then repeat for each change of time signature."))))))))
            (begin
                    (d-MoveToBeginning)
                    (d-MasterVolume 0)
                    (d-NewStructuredStaff 'initial)
                    (while (d-StaffUp))
                    (d-StaffHidden #t)
                    (d-StaffProperties "midi_channel=9")
                    (d-DirectivePut-clef-graphic "DrumClef" "DrumClef")
                    (d-DirectivePut-clef-override "DrumClef" DENEMO_OVERRIDE_GRAPHIC)
                    (d-NonPrintingStaff 'set)
                    (d-MuteStaff "unmute")
                    (d-StaffProperties (string-append "denemo_name=" DenemoClickTrack))
                    (if (d-Directive-standalone? "Upbeat")
                        (begin
                            (d-MoveCursorRight)
                            (insert-duration (duration::shortfall))
                            (d-MoveToMeasureRight)))
                    (populate-measure)))
    (d-MasterVolume old_volume)     
    (d-HighlightCursor old_highlight)
    (d-SetSaved #f))
       
