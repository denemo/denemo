;;CrossStaffArpeggio
(disp "Entered with " CrossStaffArpeggio::params " ok")
(let ((tag "CrossStaffArpeggio"))
 (if (d-Directive-staff? tag)
    (begin
        (d-DirectiveDelete-staff tag)
        (d-InfoDialog (_ "Arpeggios will not cross staffs")))
    (begin
        (d-InfoDialog (_ "Arpeggios will cross staffs"))
        (d-DirectivePut-staff-display tag (_ " Cross Staff Arpeggios "))
        (d-DirectivePut-staff-override tag  (logior DENEMO_ALT_OVERRIDE  DENEMO_OVERRIDE_AFFIX  DENEMO_OVERRIDE_GRAPHIC))
        (d-DirectivePut-staff-prefix tag " connectArpeggios = ##t "))))
(d-SetSaved #f)
