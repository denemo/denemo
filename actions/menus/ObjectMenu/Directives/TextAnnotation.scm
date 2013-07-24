;TextAnnotation
(let ((tag "TextAnnotation") (text "pizz.") (oldtext #f) (oldtag #f))
	(define (do-direction)
		(let ()					 
          (define choice (RadioBoxMenu (cons (_ "Up") 'up)  (cons (_ "Down")  'down) (cons (_ "Auto")  'auto)))
           (case choice
            ((up)  "^")
            ((down) "_")
            ((auto) "-"))))
 	(define (do-bold)
		(let ()					 
          (define choice (RadioBoxMenu (cons (_ "Bold") 'bold)  (cons (_ "Light")  'light)))
           (case choice
            ((bold)  "\\bold ")
            ((light) ""))))         
   	(define (do-italic)
		(let ()					 
          (define choice (RadioBoxMenu (cons (_ "Italic") 'italic)  (cons (_ "Normal")  'normal)))
           (case choice
            ((italic)  "\\italic ")
            ((normal) ""))))         
            
   						
	(if (equal? "edit" TextAnnotation::params)
		(set! TextAnnotation::params 'edit))
		
	(cond ((string? TextAnnotation::params)
						(let ((text  TextAnnotation::params))
							(StandaloneText tag text)
							
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
							(if text 
								
							
								(StandaloneText tag text (do-direction) (do-bold) (do-italic))))
								
					((not TextAnnotation::params)
							(d-TextAnnotation (cons 'default "pizz."))))						
							(d-RefreshDisplay)
							(d-SetSaved #f))
