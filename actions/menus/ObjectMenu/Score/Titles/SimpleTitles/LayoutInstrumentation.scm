;;;LayoutInstrumentation
(let ((tag (d-GetLayoutName)) (instrumentation #f) (data #f))
	(d-PushPosition)
	(d-GoToPosition 1 1 1 1)	
	(set! data (d-DirectiveGet-movementcontrol-data tag))
	(if data
			(set! data (car (eval-string data)))
			(set! data (string-append "\\fill-line {\\line {" tag " }}")))
	(set! instrumentation (d-GetUserInputWithSnippets (_ "Give Instrumentation") "" data))
	(if (pair? instrumentation)
		(let ((text (string-append "\\markup \\with-url #'\"scheme:(d-LayoutInstrumentation)\" { " (car instrumentation) " }")))
			
			(d-DirectivePut-movementcontrol-prefix tag text)
			(d-DirectivePut-movementcontrol-data tag (format #f "'~s" instrumentation))
			(d-DirectivePut-movementcontrol-allow tag (d-GetLayoutId))))
	(d-PopPosition))