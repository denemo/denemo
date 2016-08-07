;;;;;; CreateOssiaStaff
(let ((current "voice 1")(tag "ossia"))
  (set! current (d-StaffProperties "query=denemo_name"))
  (d-NewStructuredStaff)
  (d-SwapStaffs)
  (d-StaffProperties (string-append "denemo_name=" current "_ossia"))
  (d-DirectivePut-score-prefix "HideEmptyStaffs" 
                   "\\layout {
                                     \\context { \\RemoveEmptyStaffContext }
                                     }")
  (d-DirectivePut-staff-prefix tag "\\new Staff \\with
  {
    \\remove \"Time_signature_engraver\"
    \\remove \"Key_engraver\"
      \\remove \"Clef_engraver\"
    fontSize = #-2
   \\override StaffSymbol #'staff-space = #(magstep -2)
    firstClef = ##f
  }<<\\stopStaff")
  
  (d-DirectivePut-voice-postfix tag "\\hideNotes ")
  (d-DirectivePut-voice-display tag "Ossia")
  (d-DirectivePut-voice-override tag  DENEMO_OVERRIDE_GRAPHIC)
  (d-DirectivePut-staff-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
  (d-DirectivePut-staff-display tag "Ossia")
  (d-RefreshDisplay))
