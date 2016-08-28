;;;NewStaffBefore
(let ((name #f))
     (d-NewStructuredStaff)
     (d-SwapStaffs)
    (d-InstrumentName 'once)
    (set! name (d-DirectiveGet-staff-display "InstrumentName"))
    (if name    
            (d-StaffProperties (string-append "denemo_name=" name))))
