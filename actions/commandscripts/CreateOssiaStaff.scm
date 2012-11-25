;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;;; CreateOssiaStaff
(let ((current "voice 1"))
  (set! current (d-StaffProperties "query=denemo_name"))
  (d-AddBefore)
  (d-StaffProperties (string-append "denemo_name=" current))
  (d-DirectivePut-score-prefix "HideEmptyStaffs" 
			       "\\layout {
                                     \\context { \\RemoveEmptyStaffContext }
                                     }")
  (d-DirectivePut-staff-prefix "ossia" "\\new Staff \\with
  {
    \\remove \"Time_signature_engraver\"
    \\remove \"Key_engraver\"
      \\remove \"Clef_engraver\"
    fontSize = #-2
   \\override StaffSymbol #'staff-space = #(magstep -2)
    firstClef = ##f
  }<<{ \\stopStaff ")
 (d-DirectivePut-staff-override "ossia"  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
(d-DirectivePut-staff-display "ossia" "ossia")
(d-RefreshDisplay))
