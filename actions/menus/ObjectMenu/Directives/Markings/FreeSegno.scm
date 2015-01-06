;;;Segno attached to the next object, for example a barline. But shown between notes
(let ((tag "FreeSegno"))
   (if (d-Directive-standalone? tag)
    (if (not (d-DirectiveTextEdit-standalone tag))
     (d-DirectiveDelete-standalone tag))
    (StandAloneDirectiveProto (cons tag "\\tweak break-visibility  #begin-of-line-invisible \\mark \\markup { \\musicglyph #\"scripts.segno\" } ") #t LG-Segno "" )))

		
