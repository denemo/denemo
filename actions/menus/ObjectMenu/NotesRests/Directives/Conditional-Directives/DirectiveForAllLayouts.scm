;;;;;;;; DirectiveForAllLayouts
(let ((tag (d-DirectiveGetTag-standalone)) (text #f) (note #f))
  (if tag
     (d-ForAllLayouts #f)
     (let ((params (d-ChooseTagAtCursor)))
        (if (pair? params)
            (begin
              (set! tag (car params))
              (set! note (cdr params))
              (d-InfoDialog (string-append (_ "Directive ") tag (_ " on ") (if note (_ "Note") (_ "Chord")) (_ " will be typeset for all layouts ")))
              (if note
                (begin (d-DirectivePut-note-x tag 0)(d-DirectivePut-note-y tag 0))
                (begin (d-DirectivePut-chord-x tag 0)(d-DirectivePut-chord-y tag 0)))
              (d-SetSaved #f))
            (begin
              (d-WarningDialog (_ "No Denemo Directive to make unconditional here.")))))))
        
