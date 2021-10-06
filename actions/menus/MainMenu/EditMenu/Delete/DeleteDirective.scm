;DeleteDirective
(let ((tag/flag (d-ChooseTagAtCursor))(tag #f) (type #f))
	(if (Appending?) (d-MoveCursorLeft))
	(if tag/flag
		(if (pair? tag/flag)
			(begin
					(set! tag (car tag/flag))
					(if (Music?)
						(set! type (if (cdr tag/flag) "note" "chord"))
						(set! type 
							(if (Keysignature?) "keysig" 
							(if (Timesignature?) "timesig" 
							(if (Clef?) "clef"
							(if (d-Directive-standalone?) #f))))))
					(if type
						(begin
							(d-InfoDialog (string-append (_ "Deleted ") tag (_ " at cursor.")))
							(eval-string (string-append "(d-DirectiveDelete-" type " \"" tag "\")")))
						(d-WarningDialog (_ "Cancelled"))))
			(if (d-Directive-standalone?) 
				(begin
					(d-InfoDialog (string-append (_ "Deleted ") tag/flag (_ " at cursor.")))
					(d-DeleteObject))))
		(d-WarningDialog (_ "No Directive at Cursor Position"))))

		

