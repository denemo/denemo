;;;HideKeysig
(let ((tag "HideKeysig"))
	(if (Keysignature?)
		(if (d-Directive-keysig? tag)
		    (d-DirectiveDelete-keysig tag)
		    (begin
		        (d-DirectivePut-keysig-prefix tag  (string-append  "\\once \\override Staff.KeySignature #'stencil = ##f \\once \\override Staff.KeyCancellation #'stencil = ##f"  ))
                (d-DirectivePut-keysig-gy tag 60)
                (d-DirectivePut-keysig-graphic tag "\n⋃\nDenemo\n24")
				(SetDirectiveConditional "keysig" tag)))
		 (begin 
			(if (d-Directive-keysig? tag)
				(d-DirectiveDelete-keysig tag)
				(begin
					(d-DirectivePut-keysig-prefix tag  (string-append  "\\once \\override Staff.KeySignature #'stencil = ##f \\once \\override Staff.KeyCancellation #'stencil = ##f"  ))
					(d-DirectivePut-keysig-gy tag 60)
					(d-DirectivePut-keysig-graphic tag "\n⋃\nDenemo\n24")
					(SetDirectiveConditional "keysig" tag)))))
	(d-SetSaved #f))
