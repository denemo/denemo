;;;;;;;; DirectiveNotForLayout
(let ((params DirectiveNotForLayout::params)(tag (d-DirectiveGetTag-standalone)) ( id (d-GetLayoutId)) (text #f) (note #f))
 (define (d-InfoDialog string)
        (Help::TimedNotice (string-append string "\n") 5000))
  (if tag
     (d-NotForLayout #f)
    (begin
        (if (and (not (pair? params)) (Music?))
            (begin
                (set! params (d-ChooseTagAtCursor))
                (if params
                    (set! params (cons (cons (d-GetLayoutName) id) params))))
             (d-MakeDirectiveConditional))                   
        (if (pair? params)
            (let ((layout (car params)))
				(set! id (cdr layout))
				(set! params (cdr params))
				(set! tag (car params))
				(set! note (cdr params))
				(if note
					(d-DirectivePut-note-ignore tag id)
					(d-DirectivePut-chord-ignore tag id))
				(d-InfoDialog (string-append (_ "Directive ") "\"" tag "\"" (_ " on ") (if note (_ "Note") (_ "Chord")) (_ " will not be typeset for the layout ") "\"" (car layout) "\""))
		(d-SetSaved #f))))))
        
