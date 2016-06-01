;;;;;;;; DirectiveForAllLayouts
(let ((params DirectiveForAllLayouts::params) (tag (d-DirectiveGetTag-standalone)) (text #f) (note #f))
 (define (d-InfoDialog string)
        (Help::TimedNotice (string-append string "\n") 5000))
 (define (do-rest)
    (d-PushPosition)
    (while (d-NextObject)
        (if note
            (if (d-Directive-note? tag)
                 (begin (d-DirectivePut-note-ignore tag 0)(d-DirectivePut-note-allow tag 0)))
            (if (d-Directive-chord? tag)
                (begin (d-DirectivePut-chord-ignore tag 0)(d-DirectivePut-chord-allow tag 0)))))
    (d-PopPosition))
  (if tag
     (d-ForAllLayouts #f)
     (begin
        (if (not (pair? params))
            (set! params (d-ChooseTagAtCursor)))
        (if (pair? params)
            (begin
              (set! tag (car params))
              (set! note (cdr params))
             
              (if note
                (begin (d-DirectivePut-note-ignore tag 0)(d-DirectivePut-note-allow tag 0))
                (begin (d-DirectivePut-chord-ignore tag 0)(d-DirectivePut-chord-allow tag 0)))
                
              (if  (RadioBoxMenu
                (cons (_ "Just for this one") #f)
                (cons (_ "Apply condition to all further cases in this staff")   'yes))
                    (begin 
                        (do-rest)
                        (d-InfoDialog (string-append (_ "Directive ") "\"" tag "\"" (_ " on ") (if note (_ "Notes") (_ "Chords")) (_ " in this staff from the cursor onwards will be typeset for all layouts "))))
                    (d-InfoDialog (string-append (_ "Directive ") "\"" tag "\"" (_ " on ") (if note (_ "Note") (_ "Chord")) (_ " will be typeset for all layouts "))))
                
              (d-SetSaved #f))
            (begin
              (d-WarningDialog (_ "Cancelled")))))))
        
