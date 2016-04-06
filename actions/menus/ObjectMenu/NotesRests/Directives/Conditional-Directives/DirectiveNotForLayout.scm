;;;;;;;; DirectiveNotForLayout
(let ((params DirectiveNotForLayout::params)(tag (d-DirectiveGetTag-standalone)) ( id (d-GetLayoutId)) (text #f) (note #f)) 
 (define (do-rest)
    (d-PushPosition)
    (while (d-NextObject)
        (if note
            (if (d-Directive-note? tag)
                (d-DirectivePut-note-x tag id))
            (if (d-Directive-chord? tag)
                (d-DirectivePut-chord-x tag id))))
    (d-PopPosition))
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
              (if note
                (d-DirectivePut-note-x tag id)
                (d-DirectivePut-chord-x tag id))
                
                (if  (RadioBoxMenu
                       (cons (_ "Apply condition to all further cases in this staff")   'yes)   
                        (cons (_ "Just for this one") #f))
                     (begin
                            (do-rest)
                            (d-InfoDialog (string-append (_ "Directives ") "\"" tag "\"" (_ " on ") (if note (_ "Notes") (_ "Chords")) (_ "  in this staff from the cursor onwards will not be typeset for the layout ") "\"" (car layout) "\"" )))
                     (d-InfoDialog (string-append (_ "Directive ") "\"" tag "\"" (_ " on ") (if note (_ "Note") (_ "Chord")) (_ " will not be typeset for the layout ") "\"" (car layout) "\"")))
    
              (d-SetSaved #f))
            (begin
              (d-WarningDialog (_ "Cancelled")))))))
        
