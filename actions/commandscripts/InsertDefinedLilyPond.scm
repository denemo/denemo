;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;InsertDefinedLilyPond
(let ((directives '()) (definitions #f) (choice #f))
	(define (get-second-line text)
		(let ((thelist (string-split text #\newline)))
			(if (> (length thelist) 1)
				(list-ref thelist 1)
				"")))
	(define (extract-menuitem tag)
		(define name (get-second-line tag))
		(cons name (lambda () (d-DirectivePut-standalone name) (d-DirectivePut-standalone-postfix name (string-append "\\" name " ")) (d-DirectivePut-standalone-display
		name name)(d-DirectivePut-standalone-minpixels name 30))))
		
	(let loop ((count 1))
			(define good-tag (d-Directive-score? (string-append "Allow\n" (number->string count))))
			(if good-tag
				(begin
					(set! directives (cons good-tag directives))
					(loop (1+ count)))))
	(if (not (null? directives))
		(set! definitions (map extract-menuitem directives)))
	(if definitions
		(set! choice (d-PopupMenu definitions))
		(d-WarningDialog (_ "No Definitions have been created for this score")))
	(if choice
		(begin	
			(choice)
			(d-RefreshDisplay)
			(d-SetSaved #f))))