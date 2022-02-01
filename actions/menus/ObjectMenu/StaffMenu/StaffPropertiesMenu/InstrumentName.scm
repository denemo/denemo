;;;InstrumentName
(define-once InstrumentName::longest 0) ;;longest instrument name in score
(let ((tag  "InstrumentName") (current "") (thematch "") (indent "0.0") (size (/ (string->number (d-ScoreProperties "query=fontsize")) 10.0)) (nextmovement #f) (staff (number->string (d-GetStaff))))
(if (equal? InstrumentName::params "edit")
    (set! InstrumentName::params #f))
    
  (if (or (d-Directive-staff? "DynamicsStaff") (d-Directive-staff? "ChordStaff"))
    (begin
        (d-WarningDialog (_ "Instrument Name should not be set on a Dynamics Line or Chord Symbols"))
        (d-DirectiveDelete-staff tag))
    (begin
          (if (string? InstrumentName::params)
            (begin
                (set! current InstrumentName::params)
                (set! InstrumentName::params #f))
            (begin
                (if (d-NextMovement)
                    (begin
                        (set! nextmovement (d-GetMovement))
                        (d-PreviousMovement)))
                    ;so nextmovement is 2 if there is more than one movement and the name setting is on a staff in the first movement.      
                (set! current (d-DirectiveGet-staff-display tag ))
                (if (not current)
                    (set! current "Violin"))
                    (set! current (d-GetUserInput (_ "InstrumentName") (_ "Give name of instrument/voice/part\nfor current staff:") current))))

          (if (string? current)
             (let ((transparent-start "") (transparent-end "")(movement (number->string (d-GetMovement))))
                (d-DirectivePut-staff-display tag current)
                (d-DirectivePut-staff-override tag  (logior DENEMO_ALT_OVERRIDE  DENEMO_OVERRIDE_AFFIX  DENEMO_OVERRIDE_GRAPHIC))
                (if (equal?  current (_ "Unnamed"))
                        (begin
                            (set! transparent-start "\\transparent {")
                            (set! transparent-end "}")))
                (d-DirectivePut-staff-prefix tag  (string-append "instrumentName = \\markup {  \\with-url #'\"scheme:(d-GoToPosition " 
                        movement 
                        " " staff " 1 1)(let ((choice (d-PopupMenu (list (cons (_ \\\"Change Name\\\") d-InstrumentName)   (cons (_ \\\"Change Indent\\\") d-ScoreIndent)))))
                            (if choice (choice)))    \" " transparent-start "\"" current "\" " transparent-end"}"))

                (if (> (string-length current) 0)
                            (d-StaffProperties (string-append "denemo_name=" current))
                            (d-StaffProperties "denemo_name=unnamed"))
                    ;;if this is the first of several movements, ask to set the rest if called directly           
                (if (and (not InstrumentName::params) (equal? nextmovement 2))
                        (let ((do-all (d-GetUserInput (_ "Instrument Name") (_ "This sets the name in Movement 1\nSet this name for the same staff in other movements?") (_ "y"))))
                            (if (equal? do-all (_ "y"))
                                (let ((staffnum (d-GetStaff)))
                                    (d-PushPosition)
                                    (while (d-NextMovement)
                                        (if (d-GoToPosition #f staffnum 1 1)
                                            (d-InstrumentName current)))
                                    (d-PopPosition)))))
                (set! InstrumentName::longest 0)   
                (d-PushPosition)         
                (ForEachStaffInScore "(let ((name (d-DirectiveGet-staff-display \"InstrumentName\")))
											(if name
												(set! InstrumentName::longest (max InstrumentName::longest (string-length name)))))")
		(d-PopPosition)
                (d-ScoreIndent (* size InstrumentName::longest)))))))
        