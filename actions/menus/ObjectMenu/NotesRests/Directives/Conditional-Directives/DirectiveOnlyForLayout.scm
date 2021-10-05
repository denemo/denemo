;;;;;;;; DirectiveOnlyForLayout
(let ((params DirectiveOnlyForLayout::params)(tag (d-DirectiveGetTag-standalone)) (id (d-GetLayoutId)) (text #f) (note #f))
 (define (d-InfoDialog string)
        (Help::TimedNotice (string-append string "\n") 5000))
  (define (do-rest)
    (d-PushPosition)
    (while (d-NextObject)
        (if note
            (if (d-Directive-note? tag)
                (d-DirectivePut-note-allow tag id))
            (if (d-Directive-chord? tag)
                (d-DirectivePut-chord-allow tag id))))
    (d-PopPosition))
  (if tag
     (d-OnlyForLayout #f)
     (begin
        (if (not (pair? params))
            (begin
                (set! params (d-ChooseTagAtCursor))
                (if params
                    (set! params (cons (cons (d-GetLayoutName) id) params)))))    
        (if (pair? params)
            (let ((layout (car params)))
				  (set! id (cdr layout))
				  (set! params (cdr params))
				  (set! tag (car params))
				  (set! note (cdr params))
				 
				  (if note
					(d-DirectivePut-note-allow tag id)
					(d-DirectivePut-chord-allow tag id))
				 (d-InfoDialog (string-append (_ "Directive ") "\"" tag "\"" (_ " on ") (if note (_ "Note") (_ "Chord")) (_ " will be typeset for the layout ") "\"" (car layout) "\""))
				 (d-SetSaved #f))
            (d-WarningDialog (_ "Cancelled"))))))
        
