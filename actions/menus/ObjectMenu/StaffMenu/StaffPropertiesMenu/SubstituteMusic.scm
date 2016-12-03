;;;SubstituteMusic
(let ((params SubstituteMusic::params)(tag "SubstituteMusic"))
        (if (and (not params)(d-Directive-voice? tag))
            (begin
                (d-MoveToBeginning)
                (d-DirectiveDelete-standalone tag)
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
                                (cons (unique-staff-name)  (cons  (unique-staff-name)
                                          (cons (if (d-Directive-staff? "DynamicsStaff") "(d-DynamicsStaff 'noninteractive)" #f) (string-append "{" (d-GetPrevailingClefAsLilyPond)(d-GetPrevailingTimesigAsLilyPond)(d-GetPrevailingKeysigAsLilyPond) "\\" (d-GetVoiceIdentifier) " } \\void "))))
                                                 voicenames))      
                            (if (d-MoveToStaffDown)
                                (loop (1+ count))))
                        (d-PopPosition)
                        (reverse voicenames))
                ;;;transform to mirrored music
                (set! cuename (get-voicenames))
                (if (null? cuename)
                    (begin
                        (d-WarningDialog (_ "There are no other staffs for this one to mirror.")))
                    (begin
                        (set! cuename (RadioBoxMenuList cuename))
                        (if cuename
                            (let ((dynamics-staff (cadr cuename)))
                                (if dynamics-staff
                                    (eval-string dynamics-staff))
                                (d-DirectivePut-voice-prefix tag (cddr cuename))    
                                (d-DirectivePut-voice-override tag  (logior DENEMO_ALT_OVERRIDE DENEMO_OVERRIDE_GRAPHIC))
                                (d-DirectivePut-voice-display tag (car cuename))
                                (d-SetColorOfStaff #xF0202000)
                                (d-DirectivePut-clef-graphic tag "\nS\nDenemo\n48")
                                (d-DirectivePut-clef-gy tag 36)
                                (d-DirectivePut-clef-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND ))
                                (d-DirectivePut-keysig-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
                                (d-DirectivePut-timesig-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
                                (d-MoveToBeginning)
                                (d-Directive-standalone tag)
                                (if (not dynamics-staff) 
                                    (begin
                                        (d-DirectivePut-standalone-display tag (_ "Right click to update clef/time/key"))
                                        ;;to update clef etc
                                        (d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_DYNAMIC)))
                                (d-DirectivePut-standalone-minpixels tag 50)
                                (d-DirectivePut-standalone-graphic tag (string-append "\n"
                                (_ "Music here is mirrored from ") (d-DirectiveGet-voice-display tag) "\nDenemo\n20"))
                                ;;;(d-ToggleCurrentStaffDisplay) dynamics staffs don't display well when hidden
                                (d-SetSaved #f))))))))


