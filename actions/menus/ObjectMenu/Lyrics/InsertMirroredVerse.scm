;;;InsertMirroredVerse
(let ((params InsertMirroredVerse::params) (current (d-GetStaff)) (verse #f)(staff (d-GetStaff))(initial (d-GetVerseNumber)))
        (define (get-versenames)
                        (define versenames '())
                        (define this-movement (number->string (d-GetMovement)))
                        (define this-staff (d-GetStaff))
                        (define (unique-staff-name)
                            (string-append (d-StaffProperties "query=denemo_name") (_ " on Staff ") (number->string (d-GetStaff))))
                        
                        (while (d-MoveToStaffUp))
                        (let loop ((count 0)) 
                            (if (> current (d-GetStaff))
                                (let verseloop ((num 1))
                                    (define thisverse (d-GetVerse num))
                                   
                                    (if thisverse
                                        (let* ((len (string-length thisverse)) (short (substring thisverse 0 (min 10 len))))
                                        
                                            (set! versenames (cons 
                                                (cons (string-append (unique-staff-name) " Verse " (number->string num)  ": \"" short "...\"")
                                                     (string-append "\\" (d-GetVoiceIdentifier) "LyricsVerse" (d-RomanNumeral (number->string num)) " %" (_ "this inserts the verse ") (number->string num) ": \"" short "...\""
                                                                                                        (_ " from ") (unique-staff-name) "\n"))           
                                                             versenames))
                                            (verseloop (1+ num))))
                                
                                    (if (d-MoveToStaffDown)
                                        (loop (1+ count))))))
                        (reverse versenames))
        (set! verse (get-versenames)) 
        (if (not (= staff (d-GetStaff)))
            (while (and (d-MoveToStaffUp) (not (= staff (d-GetStaff)))))) ;;PopPosition does not alter verse
        (d-GetVerseNumber initial); 
        
         (if (null? verse)
                    (begin
                        (d-WarningDialog (_ "There are no verses above this one to mirror from.")))
                    (begin
                        (set! verse (RadioBoxMenuList verse))
                         (if verse
                            (begin
                                (if (not (d-GetVerse 1))
                                    (d-AddVerse))
                                (d-InsertTextInVerse (string-append verse "\n"))
                                (d-SetSaved #f))))))
