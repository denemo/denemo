;SeekLocations
(define-once SeekLocations::positions '())
(let ((menu-choices #f)
        (initialize #f)
        (test #f)
        (navigate #f)
        (current-staff #f)
        (condition-proc #f)
        (positions #f))
    (define (make-menu position)
        (cons (format #f "'~s" position) (format #f "'~s" position)))

    (define find-positions-for-movement   
        (lambda ()
                (initialize)
                
                (if current-staff
                    (d-GoToPosition #f current-staff 1 1)
                    (d-GoToPosition #f 1 1 1))
                (let loop ((count (d-GetStaff)))
                    (if (test)
                        (set! SeekLocations::positions (cons (GetPosition) SeekLocations::positions)))
                    (while (navigate)
                        (if (test)
                        (set! SeekLocations::positions (cons (GetPosition) SeekLocations::positions))))
                    (if (not current-staff)
                        (begin
                            (set! count (1+ count))
                            (if (d-GoToPosition #f count 1 1)
                                (loop count)))))))


    (if (null? SeekLocations::positions)
        (let (
                
                (choice (RadioBoxMenu
                            (cons (_ "Key Signature Changes") 'keychange)
                            (cons (_ "Time Signature Changes") 'timechange)
                            (cons (_ "Changes of Clef") 'clefchange)
                            (cons (string-append (_ "Notes Higher Than Note") ": " (d-GetCursorNoteWithOctave))  'higher)
                            (cons (string-append (_ "Notes Lower Than Note") ": " (d-GetCursorNoteWithOctave)) 'lower))))
                            
           (set! current-staff (if (d-MakeChoice (_ "Search locations in all staffs") (string-append (_ "Search in only in staff") ": " (number->string (d-GetStaff))) (_ "Choose where to search"))
                                    #f
                                    (d-GetStaff)))                 
                            
            (case choice
                    ((lower)
                        (set! initialize 
                            (let ((current (d-GetCursorNoteWithOctave))) 
                                (lambda ()
                                    (d-GoToPosition #f 1 1 1)
                                    (d-CursorToNote current))))        
                        (set! test (lambda ()
                            (define current (d-GetNoteAsMidi))
                            (not (or (zero? current) (>= current (d-GetCursorNoteAsMidi))))))
                        (set! navigate d-MoveCursorRight))
                    ((higher)
                        (set! initialize 
                            (let ((current (d-GetCursorNoteWithOctave))) 
                                (lambda ()
                                    (d-GoToPosition #f 1 1 1)
                                    (d-CursorToNote current))))        
                        (set! test (lambda ()
                            (define current (d-GetNoteAsMidi))
                            (not (or (zero? current) (<= current (d-GetCursorNoteAsMidi))))))
                        (set! navigate d-MoveCursorRight))                    
                    ((keychange)
                        (set! initialize (lambda () (d-GoToPosition #f 1 1 1)))
                        (set! test Keysignature?)
                        (set! navigate d-MoveCursorRight))
                    ((timechange)
                        (set! initialize (lambda () (d-GoToPosition #f 1 1 1)))
                        (set! test Timesignature?)
                        (set! navigate d-MoveCursorRight))   
                    ((clefchange)
                        (set! initialize (lambda () (d-GoToPosition #f 1 1 1)))
                        (set! test Clef?)
                        (set! navigate d-MoveCursorRight))           
                
                )
            (ForAllMovementsExecute find-positions-for-movement)))
    (set! positions SeekLocations::positions)
    
    ;(disp "positions are " positions "\n")
    (if (null? positions)
        (d-WarningDialog (_ "No locations found"))
        (begin
            (set! menu-choices (map make-menu positions))
                ;(disp "menu-choices " menu-choices " \n\nwhich is a list " (list? menu-choices) "\n\n")
            (let ((choice (TitledRadioBoxMenuList (_ "Choose a position in the score to go to (Movement, Staff, Measure, Object)") menu-choices)))
                    ;(disp  "choice " choice "which is " (string? choice) "\n")
                (if choice
                        (let ((pos (eval-string  choice)))
                            ;(disp "moving to " pos" which is " (list? pos) "\nelements are ? "  (car pos) "\n")
                            (apply d-GoToPosition pos)
                            (set! SeekLocations::positions (delete (eval-string choice) positions)))
                        (set! SeekLocations::positions '()))))))
      