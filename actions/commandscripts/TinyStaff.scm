;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;TinyStaff
(ToggleDirective "staff" "prefix" "StaffSize" "\\with {
     		   fontSize = #-7
     		   \\override VerticalAxisGroup #'minimum-Y-extent = #'(0 . 0)
     		   \\override StaffSymbol #'staff-space = #(magstep -7)\n}\n ")