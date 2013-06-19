;TextAnnotation
(let ((tag "TextAnnotation") (text "pizz.") (oldtext #f) (oldtag #f))
	(if (not (defined? 'TextAnnotation::params))
		(define TextAnnotation::params #f))
		
	(cond ((string? TextAnnotation::params)
						(let ((text  TextAnnotation::params))
							(set! tag (string-append tag "\n" text))
							(d-Directive-standalone tag)
							(d-DirectivePut-standalone-prefix tag "<>")
							(d-DirectivePut-standalone-postfix tag (string-append "-\\markup {" (scheme-escape text) " }"))
							(d-DirectivePut-standalone-grob tag "Text")
							(d-DirectivePut-standalone-display tag text)
							(d-DirectivePut-standalone-minpixels tag 30)
							))
		
					((and (pair? TextAnnotation::params) (equal? (car TextAnnotation::params) 'fontsize))
							(let ((value (cdr TextAnnotation::params)))
								(set! tag (d-DirectiveGetTag-standalone))
								(if value
									(TweakRelativeFontSize tag value))))
									
					((equal? TextAnnotation::params 'edit)
									(set! oldtag (d-DirectiveGetTag-standalone))
									(set! oldtext (d-DirectiveGet-standalone-display oldtag))
									(d-DeleteObject)
									(d-TextAnnotation (cons 'default oldtext)))
									
					((and (pair? TextAnnotation::params) (equal? (car TextAnnotation::params) 'default))
							(set! text (d-GetUserInput (_ "Text Annotation") 
														(_ "Give text to be placed in score at cursor\n(it can be dragged in the typeset view)") 
														(cdr  TextAnnotation::params)))
							(if text (d-TextAnnotation text)))
								
					((not TextAnnotation::params)
							(d-TextAnnotation (cons 'default "pizz."))))						
							(d-RefreshDisplay)
							(d-SetSaved #f))