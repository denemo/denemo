(let ((tag "HideAccidental"))
(if (d-Directive-note? tag)
	(d-DirectiveDelete-note tag)
	(begin
		(d-DirectivePut-note-prefix tag "\\tweak Accidental #'stencil ##f  ")
		(d-DirectivePut-note-display tag " x"))))
(d-SetSaved #f)
