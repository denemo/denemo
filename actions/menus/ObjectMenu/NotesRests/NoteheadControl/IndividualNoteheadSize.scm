;;;IndividualNoteheadSize
(let ((tag "NoteHeadSize") (current #f)(size #f)(data #f))
    (if (d-Directive-note? tag)
        (begin
            (set! data (d-DirectiveGet-note-data tag))))
    
    (if data
        (set! current data)
        (set! current "-2"))
    (set! size (d-GetUserInput (_ "Notehead Font Magnification") (_  "Give size required: ") current))
    (if size
        (begin
        	(if (eq? (string->number size) 0)
        		(d-DirectiveDelete-note tag)
        		(begin
			    (d-DirectivePut-note-prefix tag (string-append " \\tweak  font-size #" size " "))
			    (d-DirectivePut-note-data tag size)))
            	(d-SetSaved #f))))
