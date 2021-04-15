;;;;;;;;;;;;;;OpenAudio
(let ((seconds (d-OpenSourceAudioFile)))
    (define old_volume (d-MasterVolume)) 
    (if (and seconds (EmptyMeasure?) (not (d-MoveToMeasureRight)) (not (d-MoveToStaffDown)))
        (let ( (timesig (d-InitialTimeSig "query=timesigname")) (numerator #f)(denominator #f))
            (set! numerator (string->number (car (string-split   timesig #\/))))
            (set! denominator (string->number (cadr (string-split  timesig #\/))))
           (d-MasterVolume 0) 
            (d-CursorToNote "c'")
            (d-NonPrintingStaff 'set)
            (d-StaffProperties (string-append "denemo_name=" DenemoClickTrack))            
            (let loop ((count numerator))
                (if (> count 0)
                    (begin
                        (eval-string (string-append "(d-" (number->string (duration::lilypond->denemo denominator)) ")")) 
                        (d-SetNonprinting)
                        (d-CursorUp)
                        (loop (- count 1)))))
                        
            (d-RecreateTimebase)
 
                    
            (let loop ((count (/ seconds (d-GetMidiOffTime)))) 
                (if (> count 0)
                    (begin 
                        (d-AddDuplicateMeasure)
                        (loop (- count 1)))))
            (d-MuteStaff "unmute")      
            (d-MoveToBeginning)
            (d-MasterVolume old_volume)         
                        
            (d-SetPlaybackInterval 0.0 seconds)
                        
            (d-NewStructuredStaff))))
(d-RecreateTimebase)

