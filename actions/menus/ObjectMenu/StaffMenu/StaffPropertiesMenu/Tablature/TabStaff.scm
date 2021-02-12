;;;TabStaff
(let ((tag "TabStaff"))
	(if (equal? (d-StaffType) tag)
		(begin
			(d-StaffType "Staff")
			(d-DirectiveDelete-voice tag)
			(d-DirectiveDelete-clef tag))
		(begin
      			 (d-StaffType tag)
        		(ToggleDirective "voice" "postfix" tag "\n" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
        		(ToggleDirective "clef" "postfix" tag "{} \n"  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND)))))