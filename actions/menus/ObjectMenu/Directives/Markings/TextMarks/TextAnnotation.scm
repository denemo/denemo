;TextAnnotation
(let ((tag "TextAnnotation") (text "pizz.") (oldtext #f) (oldtag #f))
    (define (do-direction)
        (let ()                  
          (define choice (RadioBoxMenu (cons (_ "Up") 'up)  (cons (_ "Down")  'down) (cons (_ "Auto")  'auto)))
           (case choice
            ((up)  "^")
            ((down) "_")
            (else "-"))))
    (define (do-bold)
        (let ()                  
          (define choice (RadioBoxMenu (cons (_ "Bold") 'bold)  (cons (_ "Light")  'light)))
           (case choice
            ((bold)  "\\bold ")
            ((light)  "\\medium ")
            (else ""))))         
    (define (do-italic)
        (let ()                  
          (define choice (RadioBoxMenu (cons (_ "Italic") 'italic)  (cons (_ "Normal")  'normal)))
           (case choice
            ((italic)  "\\italic ")
            ((normal)  "\\upright ")
            (else ""))))         
     
                        
    (if (equal? "edit" TextAnnotation::params)
        
        (begin
                (let ((choice (RadioBoxMenu (cons (_ "Help") 'help)  (cons (_ "Edit")  'edit)  (cons (_ "Advanced")  'advanced))))  
                       (case choice
                       
                        ((edit) 
                            (set! TextAnnotation::params 'edit))
                        ((advanced) 
                            (d-DirectiveTextEdit-standalone tag)
                            (set! TextAnnotation::params 'finished))
                            
                        (else  (d-InfoDialog "This places text tied to the position of the following note or chord.\nIt can't be placed after all music, but the positioning can be altered in the typeset (print) view by right-clicking on the text.")
                            (set! TextAnnotation::params 'finished)))))
         (if (and (not TextAnnotation::params) (Appending?))
             (set! TextAnnotation::params 'abort)))
                
    (cond 
            ((equal? TextAnnotation::params 'abort) 
                     (set! TextAnnotation::params 'unchanged)
                     (d-InfoDialog (_ "This directive has to be placed before a note, chord or rest.\nThe text itself can be re-positioned by right-clicking in the typeset (print) view")))
            ((equal? TextAnnotation::params 'finished) 
                     (set! TextAnnotation::params 'unchanged))
            ((string? TextAnnotation::params)
                     (let ((text  TextAnnotation::params))
                        (StandaloneText tag text)))
        
            ((and (pair? TextAnnotation::params) (equal? (car TextAnnotation::params) 'fontsize))
                    (let ((value (cdr TextAnnotation::params)))
                        (set! tag (d-DirectiveGetTag-standalone))
                        (if value
                            (TweakRelativeFontSize tag value))))
                            
            ((equal? TextAnnotation::params 'edit)
                    (set! oldtag (d-DirectiveGetTag-standalone))
                    (set! oldtext (d-DirectiveGet-standalone-display oldtag))
                    (d-MoveCursorRight)
                    (if (Appending?)
                        (begin
                            (set! TextAnnotation::params 'unchanged)
                            (d-InfoDialog (_ "This directive must be placed before a note to work")))
                        (begin
                            (d-DeletePreviousObject)
                            (d-TextAnnotation (cons 'default oldtext)))))
                            
            ((and (pair? TextAnnotation::params) (equal? (car TextAnnotation::params) 'default))
                    (set! text (d-GetUserInput (_ "Text Annotation") 
                                                (_ "Give text to be placed in score at cursor\n(it can be dragged in the typeset view)") 
                                                (cdr  TextAnnotation::params)))
                    (if text 
                        (StandaloneText tag text (do-direction) (do-bold) (do-italic))
                        (StandaloneText tag (cdr  TextAnnotation::params) "-" "" "")))
                        
            ((not TextAnnotation::params)
                    (d-TextAnnotation (cons 'default "pizz."))))                        
(if (not (equal? TextAnnotation::params 'unchanged))
    (begin
        (d-RefreshDisplay)
        (d-SetSaved #f)))

(set! TextAnnotation::params #f))
