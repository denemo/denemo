;;;DeactivateTimeSignatureStencil
(let ((tag "DeactivateTimeSignatureStencil") (params DeactivateTimeSignatureStencil::params))
	(if (d-Directive-score? tag)
		(let ((choice (RadioBoxMenu
                (cons (_ "Delete") 'delete))))
		    (case choice
		          ((delete)
		          	(d-SetSaved #f)
		          	(d-DirectiveDelete-score tag)))) 
	(begin
		(d-SetSaved #f)
		(d-InfoDialog (_ "Time Signatures will not be typeset for this score"))
		(d-DirectivePut-score-postfix tag  "\\override Score.TimeSignature #'stencil = ##f\n"))))
