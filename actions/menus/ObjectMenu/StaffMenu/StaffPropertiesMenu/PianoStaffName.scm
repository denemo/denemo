;;;PianoStaffName
(let ((tag "PianoStaffStart") (name "Piano"))
	(if (d-DirectiveGet-staff-prefix tag)
		(begin
		    (set! name (d-GetUserInput (_ "Instrument Name") (_ "Give name of instrument for staff group starting here:") name))
		    (if name
			(set! name (string-append "\\set PianoStaff.instrumentName = #\"" name "\" "))
			(set! name ""))
		    (AttachDirective "staff"  "prefix"  "PianoStaffStart" (string-append " \\new PianoStaff << " name " \n") DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_AFFIX))
		  (begin
		  	(d-WarningDialog "This command must be issued on the first staff of a piano staff group"))))
