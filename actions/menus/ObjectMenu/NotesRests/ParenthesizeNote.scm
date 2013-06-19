 ;;; ParenthesizeNote
 (if (d-Directive-note? "Parenthesize")
 	(d-DirectiveDelete-note  "Parenthesize")
 	(begin
		(d-DirectivePut-note-prefix "Parenthesize" "\\parenthesize ")
		(d-DirectivePut-note-display "Parenthesize" "()")))
(d-SetSaved #f)