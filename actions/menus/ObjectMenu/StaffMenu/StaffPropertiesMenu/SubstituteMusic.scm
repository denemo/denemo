;;;SubstituteMusic
(let ((tag "SubstituteMusic"))
        (if (d-Directive-voice? tag)
            (begin
                (d-SetColorOfStaff 0)
                (d-DirectiveDelete-clef tag)
                (d-DirectiveDelete-keysig tag)          
                (d-DirectiveDelete-timesig tag)                     
                (d-DirectiveDelete-voice tag))
            (let ((cuename #f))
                (define (get-voicenames)
                        (define voicenames '())
                        (define this-movement (number->string (d-GetMovement)))
                        (define this-staff (d-GetStaff))
                        (define (unique-staff-name)
                            (string-append (d-StaffProperties "query=denemo_name") (_ " on Staff ") (number->string (d-GetStaff))))
                        (d-PushPosition)
                        (while (d-MoveToStaffUp))
                        (let loop ((count 0))
                            (set! voicenames (cons 
                                (cons (unique-staff-name)  (cons (unique-staff-name)
                                          (string-append "\\" (d-GetVoiceIdentifier))))
                                                 voicenames))      
                            (if (d-MoveToStaffDown)
                                (loop (1+ count))))
                        (d-PopPosition)
                        voicenames)
        ;;; procedure starts here                
                (set! cuename (get-voicenames))
                (if (null? cuename)
                    (begin
                        (d-WarningDialog (_ "There are no other staffs for this one to take a cue from.")))
                    (begin
                        (set! cuename (RadioBoxMenuList cuename))
                        (if cuename
                            (begin
                                (d-DirectivePut-voice-prefix tag (string-append (cdr cuename) " \\void "))
                                (d-DirectivePut-voice-override tag  (logior DENEMO_ALT_OVERRIDE DENEMO_OVERRIDE_GRAPHIC))
                                (d-DirectivePut-voice-display tag (car cuename))
                                (d-SetColorOfStaff #xF0202000)
                                (d-DirectivePut-clef-graphic tag "\nS\nDenemo\n48")
                                (d-DirectivePut-clef-gy tag 36)
                                (d-DirectivePut-clef-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND ))
                                (d-DirectivePut-keysig-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
                                (d-DirectivePut-timesig-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
                                (d-SetSaved #f))))))))                  

