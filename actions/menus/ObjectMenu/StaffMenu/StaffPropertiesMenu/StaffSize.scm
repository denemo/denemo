;;;StaffSize
(let ((tag "StaffSize") (size #f))
  (set! size (exact->inexact (/ (d-DirectiveGet-staff-minpixels tag) 10)))
  (set! size (d-GetUserInput (_ "Setting Staff Size") (_ "Give Staff Size (for printing), 0 for default") (if size (number->string size) "0")))

  (if (and size (string->number size))
    (begin
      (if (d-Directive-staff? tag)
        (d-DirectiveDelete-staff tag))
      (ToggleDirective "staff" "prefix" tag (string-append "
     		   fontSize = #" size "
     		   \\override VerticalAxisGroup #'minimum-Y-extent = #'(0 . 0)
     		   \\override StaffSymbol #'staff-space = #(magstep " size ")\n ") (logior DENEMO_OVERRIDE_AFFIX DENEMO_ALT_OVERRIDE))
      (set! size (inexact->exact (* (string->number size) 10)))
           
      (d-DirectivePut-staff-minpixels tag size))))
      