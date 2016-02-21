;;;;;;;; DirectiveNotForLayout
(let ((params DirectiveNotForLayout::params)(tag (d-DirectiveGetTag-standalone)) ( id (d-GetLayoutId)) (text #f) (note #f)) (disp "not for layout called with " params "\n\n")
  (if tag
     (d-NotForLayout #f)
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
              (d-InfoDialog (string-append (_ "Directive ") tag (_ " on ") (if note (_ "Note") (_ "Chord")) (_ " will not be typeset for layout ") (car layout)))
              (if note
                (d-DirectivePut-note-x tag id)
                (d-DirectivePut-chord-x tag id))
              (d-SetSaved #f))
            (begin
              (d-WarningDialog (_ "Cancelled")))))))
        
