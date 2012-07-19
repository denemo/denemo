;;; CriticalComment
(let ((tag  "CriticalComment"))
(define current (d-DirectiveGet-standalone-display tag))
(let script ((answer (d-GetUserInput "Critical Comment" "Give Comment" (if current current ""))))
	(if (and answer (not (string=? answer "")))
		(begin
			(d-Directive-standalone tag)
			(d-DirectivePut-standalone-minpixels tag 30)
			(d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_EDITOR)
			(d-DirectivePut-standalone-display tag (scheme-escape answer))
			(d-RefreshDisplay)
			(d-SetSaved #f))
		#f)))