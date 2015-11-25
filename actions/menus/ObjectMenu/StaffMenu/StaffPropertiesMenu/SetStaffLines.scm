;;;SetStaffLines
 (let ((tag "SetStaffLines") (lines #f) (params SetStaffLines::params))
    (set! lines (d-GetUserInput (_ "Staff Lines") (_ "Give Number of Staff Lines for current staff") "5"))
    (if (and (string? lines) (string->number lines))
       (begin
            (d-SetSaved #f)
            (d-SetLinesInStaff (string->number lines))
            (d-DirectivePut-voice-postfix "StaffLines"  (string-append " \\override Staff.StaffSymbol  #'line-count = #" lines " ")))
        (d-WarningDialog (_ "Cancelled"))))