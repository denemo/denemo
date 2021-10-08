;;;HideTimesig
(let ((tag "HideTimesig"))
	(if (Timesignature?)
		(if (d-Directive-clef? tag)
		    (d-DirectiveDelete-clef tag)
		    (begin
		        (d-DirectivePut-timesig-prefix tag   "%{Clef Omitted%}")
		        (d-DirectivePut-timesig-override tag DENEMO_OVERRIDE_LILYPOND)
		        (d-DirectivePut-timesig-gy tag 60)
		        (d-DirectivePut-timesig-graphic tag "\n⋃\nDenemo\n24")
		        (SetDirectiveConditional "timesig" tag)))
		 (begin ;act on initial time signature
			(if (d-Directive-timesig? tag)
				(d-DirectiveDelete-timesig tag)
				(begin
					(d-DirectivePut-timesig-prefix tag  (string-append  "\\once \\override Staff.TimeSignature #'stencil = ##f"  ))
					(d-DirectivePut-timesig-gy tag 60)
					(d-DirectivePut-timesig-graphic tag "\n⋃\nDenemo\n24")
					(SetDirectiveConditional "timesig" tag)))))
	(d-SetSaved #f))
