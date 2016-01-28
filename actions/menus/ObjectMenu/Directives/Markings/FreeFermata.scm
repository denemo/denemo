;;;Fermata attached to the next object, for example a barline. But shown between notes
(let ((tag "FreeFermata"))
   (if (d-Directive-standalone? tag)
     (let ((choice (RadioBoxMenu 
                (cons (_ "Object Inspector") 'help) 
                (cons (_ "Delete") 'delete))))
            (case choice
                ((help)
                   (d-DisplayCurrentObject))
                  ((delete)
                    (d-DirectiveDelete-standalone tag))))  
    (StandAloneDirectiveProto (cons tag "\\once \\override Score.RehearsalMark #'break-visibility =
#begin-of-line-invisible \\mark \\markup\\scale #'(0.75 . 0.75) { \\musicglyph #\"scripts.ufermata\" } "))))
        
