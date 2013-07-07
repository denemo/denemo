;;EditSimilar
  (let ((target (d-DirectiveGetTag-standalone)))
      (define (edit)
        (define choice (RadioBoxMenu
	  (cons (_ "Continue")   'continue)   
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
					((advanced) (d-DirectiveTextEdit-standalone  target))))
  (if target
    (begin
      (edit)
      (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-standalone? target))))
          (edit)))
    (begin
     	(d-InfoDialog (_ "Currently only standalone Directives are supported"))
    	#f)))
