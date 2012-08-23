;;; ChordComment
(let ()
(define current (d-DirectiveGet-chord-display "ChordComment"))
(define position (GetPosition))
(let script ((answer (d-GetUserInput (_ "Insert Comment") (_ "Comment this chord") (if current current "") #f)))
	(if (and answer (not (string=? answer "")))
		(begin
			(if (not (PositionEqual? position (GetPosition)))
				(begin
					(if (not (equal? (_ "y") (d-GetUserInput (_ "Cursor has Moved") (_ "Apply Command to new position of cursor?")  (_ "y"))))
					(apply d-GoToPosition position))))
			(d-DirectivePut-chord-override "ChordComment" DENEMO_OVERRIDE_EDITOR)
			(d-DirectivePut-chord-display "ChordComment" answer))
		#f)))