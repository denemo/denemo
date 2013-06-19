;;;Fermata attached to the next object, for example a barline. But shown between notes
(let ((tag "FreeFermata"))
   (if (d-Directive-standalone? tag)
    (if (not (d-DirectiveTextEdit-standalone tag))
     (d-DirectiveDelete-standalone tag))
    (StandAloneDirectiveProto (cons tag "\\once \\override Score.RehearsalMark #'break-visibility =
#begin-of-line-invisible \\mark \\markup { \\musicglyph #\"scripts.ufermata\" } "))))
		