;;;TinyStaff
(ToggleDirective "staff" "prefix" "StaffSize" "
     		   fontSize = #-7
     		   \\override VerticalAxisGroup #'minimum-Y-extent = #'(0 . 0)
     		   \\override StaffSymbol #'staff-space = #(magstep -7)\n "(logior DENEMO_OVERRIDE_AFFIX DENEMO_ALT_OVERRIDE))