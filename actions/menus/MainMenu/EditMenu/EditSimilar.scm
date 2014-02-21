;;EditSimilar
(let ((target #f))
   (define (get-tag-list get-command) ;;;get-command is d-DirectiveGetNthTag-note
    (let loop ((tags '())(n 0))
        (define tag #f)
        (set! tag (get-command n))
        (if tag
            (begin 
                (set! tags (cons tag tags))
                (loop tags (+ 1 n)))
            tags)))
        
  (define (select-directive type)
    (let ((tags '()))
        (case type
            ((note)
                (set! tags (get-tag-list d-DirectiveGetNthTagStrictNote)))
            ((chord)
                (set! tags (get-tag-list d-DirectiveGetNthTag-chord)))) 
    (if (null? tags)
            #f
        (if (> (length tags) 1)
            (RadioBoxMenuList tags)
            (list-ref tags 0)))))
  
 (define (edit-tag tag default-action)
    (let ((command (with-input-from-string (string-append "d-"  tag) read)))
        (if (defined? command)
            ((eval command (current-module)) "edit")
            (default-action tag))))
  (define (edit)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " Directives"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
          (case choice
            ((delete) (d-DirectiveDelete-standalone target))
            ((edit) (d-EditObject))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-standalone  target))
            ((#f)  (set! target #f))))

  (define (edit-note)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Noteheads"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
          (case choice
            ((delete) (d-DirectiveDelete-note target))
            ((edit) (edit-tag target d-DirectiveTextEdit-note))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-note target))
            ((#f)  (set! target #f)))
            (d-CursorUp))

  (define (edit-chord)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Chords/Notes/Rests"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
          (case choice
            ((delete) (d-DirectiveDelete-chord target))
            ((edit) (edit-tag target d-DirectiveTextEdit-chord))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-chord target))
            ((#f)  (set! target #f))))

;;; the actual procedure
  (if EditSimilar::params 
    (if (eq? (cdr EditSimilar::params) 'standalone)
        (begin
            (set! target (car EditSimilar::params))
            (FindNextObjectAllColumns (lambda () (d-Directive-standalone? target)))))
    (set! target (d-DirectiveGetTag-standalone)))
  (if target
    (begin
      (edit)
      (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-standalone? target))))
          (edit)))
    (begin
      (if EditSimilar::params 
        (if (eq? (cdr EditSimilar::params) 'note)
            (begin
                (set! target (car EditSimilar::params))
                (FindNextNoteAllColumns (lambda () (d-DirectiveGetForTagStrictNote target)))))
        (set! target (select-directive 'note)))
      (if target
        (begin
          (edit-note)
          (while (and target (FindNextNoteAllColumns (lambda () (d-DirectiveGetForTagStrictNote target))))
              (edit-note)))
        
        (begin
            (if EditSimilar::params 
                (if (eq? (cdr EditSimilar::params) 'chord)
                    (begin 
                        (set! target (car EditSimilar::params))
                        (FindNextObjectAllColumns (lambda () (d-Directive-chord? target)))))
                (set! target (select-directive 'chord)))
          (if target
            (begin
              (edit-chord)
              (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-chord? target))))
                  (edit-chord)))
            (d-InfoDialog (_ "Currently only Directives attached to noteheads, chords (including notes and rests) or standalone are supported - position the cursor on a notehead for directives on that notehead or off the noteheads for directives on a chord/note/rest, or on a standalone directive. \nAlternatively, use \"Choose, Seek & Edit\" to select from possible directives in your score."))))))))
