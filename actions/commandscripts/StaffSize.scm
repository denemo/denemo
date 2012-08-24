;;;StaffSize
(let ((tag "StaffSize") (size (d-GetUserInput (_ "Setting Staff Size") (_ "Give Staff Size (for printing), 0 for default") "0")))
(if (and size (string->number size))
  (begin
    (if (d-Directive-staff? tag)
      (d-DirectiveDelete-staff tag))
    (ToggleDirective "staff" "prefix" tag (string-append "\\with {
     		   fontSize = #" size "
     		   \\override VerticalAxisGroup #'minimum-Y-extent = #'(0 . 0)
     		   \\override StaffSymbol #'staff-space = #(magstep " size ")\n}\n ")))))