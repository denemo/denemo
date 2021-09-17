(let ((tag "GroupStaffStart"))
(if (and (or  (d-Directive-staff? "MarksStaff")  (d-Directive-staff? "DynamicsStaff")) (d-MoveToStaffDown))
    (d-GroupStaffStart)
    (begin	
	    (if (d-Directive-staff? tag)
		(d-DirectiveDelete-staff tag)
		(AttachDirective "staff" "prefix" `(,tag . "Group Staff Start") " \\new StaffGroup <<\n" DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_AFFIX DENEMO_OVERRIDE_TAGEDIT)))))
