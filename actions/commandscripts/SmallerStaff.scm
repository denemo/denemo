;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SmallerStaff
(ToggleDirective "staff" "prefix" "StaffSize" "\\with {\n
     		   fontSize = #-3\n
     		   \\override StaffSymbol #'staff-space = #(magstep -3)\n}\n " )
          		   