;;; Comment
(let ((tag "Comment")(params Comment::params))
(define current (d-DirectiveGet-standalone-display tag))
(define position (GetPosition))
	(if (equal? params "edit")
		(begin
			(d-WarningDialog (string-append (_ "Comment is currently set to: \n") current))
			(set! params #f)))
	(if params
			(set! current params)
			(set! current (d-GetUserInput (_ "Insert Comment") (_ "Give comment text ") (if current current "") #f)))
	(if (and current (not (string-null? current)))
			(begin
				(if (not (PositionEqual? position (GetPosition)))
					(begin
						(if (not (equal? (_ "y") (d-GetUserInput (_ "Cursor has Moved") (_ "Insert comment at new position of cursor?")  (_ "y"))))
						(apply d-GoToPosition position))))
				(d-Directive-standalone tag)
				(d-DirectivePut-standalone-minpixels tag 30)	
				(d-DirectivePut-standalone-graphic tag "\nC\nDenemo\n24")		
				(d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_EDITOR)
				(d-DirectivePut-standalone-display tag current)
				(d-SetSaved #f)
				(d-RefreshDisplay))))
				
