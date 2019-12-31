;;;DeactivateTimeSignatureStencil
(let ((tag "DeactivateTimeSignatureStencil") (params DeactivateTimeSignatureStencil::params))
	(if (eq? params 'delete)
		(d-DirectiveDelete-score tag)
		(if (eq? params 'install)
			(d-DirectivePut-score-postfix tag  "\\override Score.TimeSignature #'stencil = ##f\n")
			(if (d-Directive-score? tag)
				(d-DirectiveDelete-score tag)
				(begin
					(d-InfoDialog (_ "Time Signatures will not be typeset for this score"))
					(d-DirectivePut-score-postfix tag  "\\override Score.TimeSignature #'stencil = ##f\n")))))
	(d-SetSaved #f))