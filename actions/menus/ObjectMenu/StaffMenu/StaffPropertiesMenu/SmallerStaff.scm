;;;SmallerStaff
(ToggleDirective "staff" "prefix" "StaffSize" "
     		   fontSize = #-3\n
     		   \\override StaffSymbol #'staff-space = #(magstep -3)\n "  (logior DENEMO_OVERRIDE_AFFIX DENEMO_ALT_OVERRIDE))
          		   