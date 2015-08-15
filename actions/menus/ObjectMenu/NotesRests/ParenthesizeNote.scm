;;; ParenthesizeNote
 (let ((tag  "ParenthesizeNote"))
   (if (d-Directive-note? tag)
 	(let ((choice (RadioBoxMenu 
                (cons (_ "Object Inspector") 'help) 
                (cons (_ "Delete") 'delete))))
            (case choice
                ((help)
                   (d-DisplayCurrentObject))
                  ((delete)
                    (d-DirectiveDelete-note tag)))) 
 	(begin
		(d-DirectivePut-note-prefix tag "\\parenthesize ")
		(d-DirectivePut-note-display tag "()")))
  (d-SetSaved #f)
  (d-RefreshDisplay))