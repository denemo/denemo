(let ((tag "ChoirStaffStart"))
(if (and (or  (d-Directive-staff? "MarksStaff")  (d-Directive-staff? "DynamicsStaff")) (d-MoveToStaffDown))
    (d-ChoirStaffStart)
    (begin	
	    (if (d-Directive-staff? tag)
		(d-DirectiveDelete-staff tag)
		(AttachDirective "staff" "prefix" `(,tag . "Choir Staff Start") " \\new ChoirStaff <<\n" DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_AFFIX DENEMO_OVERRIDE_TAGEDIT)))))
