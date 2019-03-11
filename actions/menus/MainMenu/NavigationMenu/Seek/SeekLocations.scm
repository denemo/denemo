;SeekLocations
(define-once SeekLocations::positions '())
(define-once SeekLocations::type "")
(let ((menu-choices #f)
        (initialize #f)
        (test #f)
        (navigate #f)
        (current-staff #f)
        (condition-proc #f)
        (positions #f))
    (define (make-menu position)
        (cons (format #f "(Mvmnt, Staff/Voice, Measure, Object) = ~s" position) (format #f "'~s" position)))

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
        (let ((menu-list '())(choice #f))
           (set! current-staff (if (d-MakeChoice (_ "Search locations in all staffs") (string-append (_ "Search in only in staff") ": " (number->string (d-GetStaff))) (_ "Choose where to search"))
                                    #f
                                    (d-GetStaff)))  
            (set! menu-list (cons*
                            (cons (_ "Key Signature Changes") 'keychange)
                            (cons (_ "Time Signature Changes") 'timechange)
                            (cons (_ "Changes of Clef") 'clefchange)
                            (cons (_ "Custom") 'custom)
                            menu-list))
                            
            (set! menu-list (cons* 
                            (cons (string-append (_ "Notes Higher Than Note") ": " (d-GetCursorNoteWithOctave))  'higher)
                            (cons (string-append (_ "Notes Lower Than Note") ": " (d-GetCursorNoteWithOctave)) 'lower)
                            menu-list))
            (let ((tag (d-DirectiveGetForTag-standalone)))
                (if tag
                    (set! menu-list (cons* (cons (string-append (_ "Denemo Directives tagged") ": " tag) 'directive) menu-list))))
                
            (set! choice (RadioBoxMenuList menu-list))
            
            (case choice
                    ((lower)
                        (set! SeekLocations::type (string-append (_ "Note lower than") ": " (d-GetCursorNoteWithOctave)))
                        (set! initialize 
                            (let ((current (d-GetCursorNoteWithOctave))) 
                                (lambda ()
                                    (d-GoToPosition #f 1 1 1)
                                    (d-CursorToNote current))))        
                        (set! test (lambda ()
                            (define current (d-GetNoteAsMidi))
                            (not (or (zero? current) (>= current (d-GetCursorNoteAsMidi))))))
                        (set! navigate d-NextNote))
                    ((higher)
                        (set! SeekLocations::type (string-append (_ "Note higher than") ": " (d-GetCursorNoteWithOctave)))

                        (set! initialize 
                            (let ((current (d-GetCursorNoteWithOctave))) 
                                (lambda ()
                                    (d-GoToPosition #f 1 1 1)
                                    (d-CursorToNote current))))        
                        (set! test (lambda ()
                            (define current (d-GetNoteAsMidi))
                            (not (or (zero? current) (<= current (d-GetCursorNoteAsMidi))))))
                        (set! navigate d-NextNote))                    
                    ((keychange)
                        (set! SeekLocations::type (_ "Key Changes"))
                        (set! initialize (lambda () (d-GoToPosition #f 1 1 1)))
                        (set! test Keysignature?)
                        (set! navigate d-NextObject))
                    ((timechange)
                        (set! SeekLocations::type (_ "Time Signature Changes"))
                        (set! initialize (lambda () (d-GoToPosition #f 1 1 1)))
                        (set! test Timesignature?)
                        (set! navigate d-NextObject))   
                    ((clefchange)
                        (set! SeekLocations::type (_ "Clef Changes"))
                        (set! initialize (lambda () (d-GoToPosition #f 1 1 1)))
                        (set! test Clef?)
                        (set! navigate d-NextObject))           
                    ((directive)
                        (set! SeekLocations::type (string-append (_ "Denemo Directives tagged") ": " (d-DirectiveGetForTag-standalone)))
                        (set! initialize (lambda () (d-GoToPosition #f 1 1 1)))
                        (set! test  (let ((tag (d-DirectiveGetForTag-standalone))) (lambda () (d-DirectiveGetForTag-standalone tag))))
                        (set! navigate d-NextObject)) 
                        
                     ((custom)
                        (let ((scheme (d-GetSchemeText)))
                            (if (string-null? scheme)
                                (begin
                                    (set! choice #f)
                                    (d-WarningDialog (_ "You need a Scheme expression Scheme Script window\nto use this option\nLocations where the expression evaluates are true will be found."))
                                    (if (d-MakeChoice (_ "Install a template script in the Scheme Script window") (_ "Cancel") (_ "Seek Locations"))
                                        (begin
                                            (d-AppendSchemeText ";create your own condition here, evaluating to anything not #f (i.e. not false) at locations desired\n;Here is an example, which will choose all locations where there is a tied note\n     (d-IsTied)\n")
                                            (d-WarningDialog (_ "Open View->Scheme Script to edit the script to test for the locations you want to find")))))
                                (begin
                                   (set! initialize (lambda () (d-GoToPosition #f 1 1 1)))
                                    (set! test   (eval-string (string-append "(lambda () " scheme ")")))
                                    (set! navigate d-NextObject)))
                            (set! SeekLocations::type (_ "Scheme Script")))))
                            
                            
    (if choice
                (ForAllMovementsExecute find-positions-for-movement))))
    (set! positions SeekLocations::positions)
    
    ;(disp "positions are " positions "\n")
    (if (null? positions)
        (d-WarningDialog (_ "No locations found"))
        (begin
            (set! menu-choices (map make-menu positions))
                ;(disp "menu-choices " menu-choices " \n\nwhich is a list " (list? menu-choices) "\n\n")
            (let ((choice (TitledRadioBoxMenuList (string-append SeekLocations::type ": " (_ "Choose a location to go to (Movement, Staff, Measure, Object) and click OK")) (reverse menu-choices))))
                    ;(disp  "choice " choice "which is " (string? choice) "\n")
                (if choice
                        (let ((pos (eval-string  choice)))
                            ;(disp "moving to " pos" which is " (list? pos) "\nelements are ? "  (car pos) "\n")
                            (apply d-GoToPosition pos)
                            (set! SeekLocations::positions (delete (eval-string choice) positions)))
                        (set! SeekLocations::positions '()))))))
      