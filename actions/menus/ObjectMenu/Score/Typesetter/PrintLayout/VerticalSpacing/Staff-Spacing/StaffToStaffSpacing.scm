;StaffToSpacing
(let ((choice (RadioBoxMenu (cons (_ "Spacing for Un-grouped staffs (no braces)") "UngroupedStaffSpacing")
                            (cons (_ "Spacing for Piano Staff") "PianoStaff")
                            (cons (_ "Spacing for Staff Group") "StaffGroup")
                            (cons (_ "Spacing for Choir Staff") "ChoirStaff")
                            (cons (_ "Spacing for Grand Staff") "GrandStaff")))
        (tag #f)(context #f))
        
    (define (choose-parameter)
            (RadioBoxMenu (cons (_ "Basic Distance Apart") "basic-distance")
                          (cons (_ "Minimum Distance Apart") "minimum-distance")
                          (cons (_ "Padding") "padding")
                          (cons (_ "Stretchabilty") "stretchabilty")))
    (define (get-value tag parameter)
        (let ((current (d-DirectiveGet-score-data tag)))
            (if (not current)
                (set! current "10"))
                (set! current (d-GetUserInput (_ "Staff Spacing") (string-append (_ "Give ") parameter) current))
                (if (and current (string->number current))
                    current
                    #f)))
                
    (if choice
            (begin
                (set! context choice)
                (set! tag (string-append context "Spacing"))
                (if (equal? choice "UngroupedStaffSpacing")
                    (let ((parameter (choose-parameter)))
                         (set! tag (string-append tag "-" choice "-" parameter))
                         (let ((current (get-value tag parameter)))
                            (if current
                                (begin
                                    (d-DirectivePut-score-prefix tag (string-append 
                                                                    "\\layout {\\context {\\Staff \\override VerticalAxisGroup.default-staff-staff-spacing." parameter " = " current "}\n     }\n"))
                                                    (d-DirectivePut-score-data tag current)))))
                    (begin ;grouped staff spacing
                        (set! choice (RadioBoxMenu (cons (_ "Spacing between group and other") "staffgroup")
                                     (cons (_ "Spacing within group") "staff")))
                        (if choice
                            (let ((parameter (choose-parameter)))
                                (if parameter
                                    (begin
                                        (set! tag (string-append tag "-" choice "-" parameter))
                                        (let ((current (get-value tag parameter)))
                                            (if current
                                                (begin
                                                    (d-DirectivePut-score-prefix tag (string-append 
                                                                    "\\layout {\\context {\\" context " \\override StaffGrouper." choice "-staff-spacing." parameter " = " current "}\n     }\n"))
                                                    (d-DirectivePut-score-data tag current)))))))))))))
(d-SetSaved #f)
