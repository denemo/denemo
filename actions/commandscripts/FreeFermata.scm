;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;Fermata attached to the next object, for example a barline. But shown between notes
(let ((tag "FreeFermata"))
   (if (d-Directive-standalone? tag)
	(d-DirectiveTextEdit-standalone tag)
	(StandAloneDirectiveProto (cons tag "\\once \\override Score.RehearsalMark #'break-visibility =
#begin-of-line-invisible \\mark \\markup { \\musicglyph #\"scripts.ufermata\" } "))))
		