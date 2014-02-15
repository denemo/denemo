;;EditSimilar
(let ((target #f))

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
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Notes"))   'continue)   
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
            ((#f)  (set! target #f))))

  (define (edit-chord)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Chords"))   'continue)   
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
  (set! target (d-DirectiveGetTag-standalone))
  (if target
    (begin
      (edit)
      (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-standalone? target))))
          (edit)))
    (begin
      (set! target (d-DirectiveGetTag-note))
      (if target
        (begin
          (edit-note)
          (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-note? target))))
              (edit-note)))
        
        (begin
          (set! target (d-DirectiveGetTag-chord))
          (if target
            (begin
              (edit-chord)
              (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-chord? target))))
                  (edit-chord)))
            (d-InfoDialog (_  "Currently only  Directives are supported - position the cursor on a notehead for that note or off the noteheads for the chord."))))))))
