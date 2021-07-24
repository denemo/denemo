;;;CreateClickStaffForMidi
(let ((params CreateClickStaffForMidi::params))
 (define (writeBar numerator denominator)
    (let loop ((count numerator))
      (if (positive? count)
      (begin
        (eval-string (string-append "(d-" (number->string (duration::lilypond->denemo denominator)) ")"))
        (loop (- count 1))))))
        
  (define (writeBars count numer denom)
    (if (positive? count)
                    (begin         
                        (writeBar numer denom)
                        (writeBars (1- count)  numer denom))))
    
 (define (writeAllBars duration tempo old_tempo)    
    (if (and duration tempo)
        (let* ( (numer (list-ref tempo 1))  
                (denom (list-ref tempo 2)) 
                (spqn (list-ref tempo 3))
                (seconds_per_bar (* spqn (* numer (* (/ 4 denom)))))
                (bars (round (/ duration seconds_per_bar)))
                (thetimesig (string-append (number->string numer) "/" (number->string denom))))
            
            (if (not (None?))
                            (d-AddMeasure))
            (if (not (equal? (d-GetPrevailingTimesig) thetimesig))
                (d-InsertTimeSig thetimesig))
            (if (or (not old_tempo) (not (equal? (list-ref tempo 3) (list-ref old_tempo 3))))
                (let ((tag "MetronomeMark")(bpm (* 60 (/ 1 (list-ref tempo 3)))))
                    (d-DirectivePut-standalone tag)
                       (d-DirectivePut-standalone-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_TEMPO DENEMO_OVERRIDE_STEP))
                    (d-DirectivePut-standalone-midibytes tag (number->string bpm))
                    (d-DirectivePut-standalone-display tag (string-append  (number->string (round bpm)) " = 𝅘𝅥 "  ))
                    (d-DirectivePut-standalone-minpixels tag 30)
                    (d-MoveCursorRight)
                ))
                (writeBars bars numer denom))))
                        
    (define duration #f)
    (define old-time 0)
    (define tempo #f)    
    (define old_highlight (d-HighlightCursor #f))
    (define old_volume (d-MasterVolume))
    (define next-tempo #f)
    (define old_tempo #f)
    ;;; the procedure
    (if  (d-GetImportedMidiTracks)
        (begin
            (d-MasterVolume 0)
            (d-AddInitial)
            (d-NonPrintingStaff 'set)
            (d-CursorToNote "c'")
            (d-StaffProperties (string-append "denemo_name=" DenemoClickTrack))
            (let loop ((count 0))
                (set! tempo (d-GetRecordedMidiTempo count))
                (set! next-tempo #f)
                (if tempo
                    (begin
                        (set! next-tempo (d-GetRecordedMidiTempo (1+ count)))
                        (if next-tempo
                            (begin
                                (set! duration (- (list-ref next-tempo 0) old-time))
                                (set! old-time  (list-ref next-tempo 0)))
                            (begin
                                (set! duration (- (d-GetRecordedMidiDuration) old-time)))))     
                    (begin
                        (set! duration (d-GetRecordedMidiDuration))))
                (writeAllBars duration tempo old_tempo)
                (set! old_tempo tempo)
                (if next-tempo              
                    (loop (1+ count))))
        (d-MuteStaff "unmute"))
        ;;No MIDI file loaded - create click staff for recording
        (let ((number "16") (timesig (d-InitialTimeSig "query=timesigname")))
            (define numerator (car (string-split   timesig #\/)))
            (define denominator (cadr (string-split  timesig #\/)))
			(if params 
				(set! number (if (string? params) params (number->string params)))
				(set! number (d-GetUserInput (_ "Click Track Creation") (_ "Give number of measures required: ") number)))
            (if (and number (string->number number))
                (begin
                    (d-MasterVolume 0)
                    (d-AddInitial)
                    (d-CursorToNote "c'")
                    (d-NonPrintingStaff 'set)
                    (d-StaffProperties (string-append "denemo_name=" DenemoClickTrack))
                    (writeBars (string->number number) (string->number numerator)(string->number denominator))
                    (d-MuteStaff "unmute"))
                (d-WarningDialog (_ "Cancelled")))))
    (d-MoveToBeginning)
    (d-MasterVolume old_volume)     
    (d-HighlightCursor old_highlight))
            
