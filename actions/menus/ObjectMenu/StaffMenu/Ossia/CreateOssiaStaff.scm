;;;;;; CreateOssiaStaff
(let ((current "voice 1")(tag "ossia"))
  (set! current (d-StaffProperties "query=denemo_name"))
  (d-NewStructuredStaff)
  (d-SwapStaffs)
  (d-StaffProperties (string-append "denemo_name=" current "_ossia"))
   (if (not (d-Directive-layout? "HideEmptyStaffsAllSystems"))
  	(d-HideEmptyStaffsAllSystems))
  (if (not (d-Directive-layout? "HideEmptyStaffs"))
  	(d-HideEmptyStaffs))

  (d-DirectivePut-staff-prefix tag "
    \\remove \"Time_signature_engraver\"
    \\hide Staff.KeySignature
      \\remove \"Clef_engraver\"
    fontSize = #-2
   \\override StaffSymbol #'staff-space = #(magstep -2)
    firstClef = ##f
  ")
  
  (d-DirectivePut-voice-postfix tag "\\stopStaff \\hideNotes ")
  (d-DirectivePut-voice-display tag "Ossia")
  (d-DirectivePut-voice-override tag  DENEMO_OVERRIDE_GRAPHIC)
  (d-DirectivePut-staff-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE DENEMO_OVERRIDE_AFFIX))
  (d-DirectivePut-staff-display tag "Ossia")
  (d-RefreshDisplay))
