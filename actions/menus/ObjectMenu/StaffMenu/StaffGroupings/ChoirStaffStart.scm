(let ((tag "ChoirStaffStart"))
    (if (d-Directive-staff? tag)
        (d-DirectiveDelete-staff tag)
        (AttachDirective "staff" "prefix" `(,tag . "Choir Staff Start") " \\new ChoirStaff <<\n" DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_AFFIX DENEMO_OVERRIDE_TAGEDIT)))
