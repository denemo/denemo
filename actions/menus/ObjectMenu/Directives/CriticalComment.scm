;; CriticalComment
(let ((tag  "CriticalComment") (lilypond ""))
(define current (d-DirectiveGet-standalone-display tag))
(define position (GetPosition))
(if current
	(set! current (substring current (string-length (GetNthLine current 0)))))
;(disp "we have " current " now\n\n")
(let script ((answer (d-GetUserInputWithSnippets (_ "Critical Comment") (_ "Give Comment") (if current current "")  #f )))
    (if answer
    	(begin
	(set! lilypond (cdr answer))
	(set! answer (car answer))
	(if (not (PositionEqual? position (GetPosition)))
		(begin
			(if (not (equal? (_ "y") (d-GetUserInput (_ "Cursor has Moved") (_ "Apply Command to new position of cursor?")  (_ "y"))))
			(apply d-GoToPosition position))))))
	(if (and answer (not (string=? answer "")))
		(begin
			(d-Directive-score "CriticalCommentsAmended")
			(d-Directive-standalone tag)
			(d-DirectivePut-standalone-minpixels tag 30)
			(d-DirectivePut-standalone-override tag
				(logior DENEMO_OVERRIDE_HIDDEN DENEMO_OVERRIDE_EDITOR))
			(d-DirectivePut-standalone-display tag  (string-append (_ "Critical Comment") "\n" answer))
			(d-DirectivePut-standalone-postfix tag lilypond)
			(d-RefreshDisplay)
			(d-SetSaved #f))
		#f)))
		