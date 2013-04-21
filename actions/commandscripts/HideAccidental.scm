;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(let ((tag "HideAccidental"))
(if (d-Directive-note? tag)
	(d-DirectiveDelete-note tag)
	(begin
		(d-DirectivePut-note-prefix tag "\\tweak Accidental #'stencil ##f  ")
		(d-DirectivePut-note-display tag " x"))))
(d-SetSaved #f)
