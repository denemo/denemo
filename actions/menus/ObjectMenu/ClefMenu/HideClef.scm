;;;HideClef
(let ((tag "HideClef"))
	(if (Clef?)
		(if (d-Directive-clef? tag)
		    (d-DirectiveDelete-clef tag)
		    (begin
		        (d-DirectivePut-clef-prefix tag   "%{Clef Omitted%}")
		        (d-DirectivePut-clef-override tag DENEMO_OVERRIDE_LILYPOND)
		        (d-DirectivePut-clef-gy tag 60)
		        (d-DirectivePut-clef-graphic tag "\n⋃\nDenemo\n24")
		        (SetDirectiveConditional "clef" tag)))
		 (begin ;;;act on initial Clef
			(if (d-Directive-clef? tag)
				(d-DirectiveDelete-clef tag)
				(begin
					(d-DirectivePut-clef-prefix tag  (string-append  "\\once \\override Staff.Clef #'stencil = ##f"  ))
					(d-DirectivePut-clef-gy tag 60)
					(d-DirectivePut-clef-graphic tag "\n⋃\nDenemo\n24")
					(SetDirectiveConditional "clef" tag)))))
	(d-SetSaved #f))
