(let ((tag "GrandStaffStart"))
(if (and (or  (d-Directive-staff? "MarksStaff")  (d-Directive-staff? "DynamicsStaff")) (d-MoveToStaffDown))
    (d-GrandStaffStart)
    (begin	
	    (if (d-Directive-staff? tag)
		(d-DirectiveDelete-staff tag)
		(AttachDirective "staff" "prefix" `(,tag . "Grand Staff Start") " \\new GrandStaff <<\n" DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_AFFIX DENEMO_OVERRIDE_TAGEDIT)))))
