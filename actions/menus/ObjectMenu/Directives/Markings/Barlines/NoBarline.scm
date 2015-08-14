;;NoBarline
(let ((tag "NoBarline"))
   (if (d-Directive-standalone? tag)
     (let ((choice (RadioBoxMenu 
                (cons (_ "Object Inspector") 'help) 
                (cons (_ "Delete") 'delete))))
            (case choice
                ((help)
                   (d-DisplayCurrentObject))
                  ((delete)
                    (d-DirectiveDelete-standalone tag))))
       (begin 
       		(StandAloneDirectiveProto (cons tag "\\bar \"\"") #f "\n⋂\nDenemo\n24")
			;(StandAloneSelfEditDirective (cons tag "\\bar \"\"") #f tag)
		(d-DirectivePut-standalone-graphic tag "\n⋂\nDenemo\n24")
		(d-DirectivePut-standalone-gx tag 20)			
		(d-DirectivePut-standalone-gy tag -20))))