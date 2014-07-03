;; CriticalComment
(let* ((tag  "CriticalComment") (lilypond "")
	( current (d-DirectiveGet-standalone-data tag))
	( position (GetPosition)))
(if (not current) ;;;backward compatibilty
	(begin 
		(set! current (d-DirectiveGet-standalone-display tag))
		(if current
			(set! current (substring current (string-length (GetNthLine current 0)))))))

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
			(d-DirectivePut-standalone-display tag  (_ "Critical Comment"))
			(d-DirectivePut-standalone-data tag answer)
			(d-DirectivePut-standalone-postfix tag lilypond)
			(d-RefreshDisplay)
			(d-SetSaved #f))
		#f)))
		