;;; ParenthesizeNote
 (let ((tag  "Parenthesize"))
   (if (d-Directive-note? tag)
 	(d-DirectiveDelete-note  tag)
 	(begin
		(d-DirectivePut-note-prefix tag "\\parenthesize ")
		(d-DirectivePut-note-display tag "()")))
  (d-SetSaved #f)
  (d-RefreshDisplay))