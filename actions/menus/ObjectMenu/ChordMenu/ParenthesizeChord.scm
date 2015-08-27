;;; ParenthesizeChord
(let ((tag "ParenthesizeChord"))
 (if (d-Directive-chord? tag)
 	(d-DirectiveDelete-chord  tag)
 	(begin
		(d-DirectivePut-chord-prefix tag "\\parenthesize ")
		(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
		(d-DirectivePut-chord-display tag "()")))
(d-SetSaved #f))