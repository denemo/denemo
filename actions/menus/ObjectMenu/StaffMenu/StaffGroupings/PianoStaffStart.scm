(let ((tag "PianoStaffStart"))
(if (and (or  (d-Directive-staff? "MarksStaff")  (d-Directive-staff? "DynamicsStaff")) (d-MoveToStaffDown))
    (d-PianoStaffStart)
    (begin	
	    (if (d-Directive-staff? tag)
		(d-DirectiveDelete-staff tag)
		(AttachDirective "staff" "prefix" `(,tag . "Piano Staff Start") " \\new PianoStaff <<\n" DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_AFFIX DENEMO_OVERRIDE_TAGEDIT)))))
