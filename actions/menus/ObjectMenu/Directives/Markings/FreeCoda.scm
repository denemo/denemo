;;;Coda symbol attached to the next object, for example a barline. But shown between notes
(let ((tag "FreeCoda"))
   (if (d-Directive-standalone? tag)
         (let ((choice (RadioBoxMenu 
                (cons (_ "Object Inspector") 'help) 
                (cons (_ "Delete") 'delete))))
            (case choice
                ((help)
                   (d-DisplayCurrentObject))
                  ((delete)
                    (d-DirectiveDelete-standalone tag))))
     (let ((choice (RadioBoxMenu
     		(cons (_ "Show if at end of line") "begin")
     		(cons	(_ "Show if at start of line")  "end"))))
     		(if choice  
    		(StandAloneDirectiveProto (cons tag 
    		(string-append "\\tweak break-visibility  #" choice "-of-line-invisible \\mark \\markup\\scale #'(1 . 1) { \\musicglyph #\"scripts.coda\" } ")) #t LG-Coda "" )))))

        
