;;;NewStaffAfter
(let ((name #f))
    (d-NewStructuredStaff)
    (d-InstrumentName 'once)
    (set! name (d-DirectiveGet-staff-display "InstrumentName"))
    (if name    
            (d-StaffProperties (string-append "denemo_name=" name))))
