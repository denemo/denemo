;;;;;;;; DirectiveNotForLayout
(let ((params DirectiveNotForLayout::params)(tag (d-DirectiveGetTag-standalone)) ( id (d-GetLayoutId)) (text #f) (note #f))
  (if tag
     (d-NotForLayout #f)
     (begin
        (if (not (pair? params))
            (set! params (d-ChooseTagAtCursor)))
        (if (pair? params)
            (begin
              (set! tag (car params))
              (set! note (cdr params))
              (d-InfoDialog (string-append (_ "Directive ") tag (_ " on ") (if note (_ "Note") (_ "Chord")) (_ " will not be typeset for layout ") (d-GetLayoutName)))
              (if note
                (d-DirectivePut-note-x tag id)
                (d-DirectivePut-chord-x tag id))
              (d-SetSaved #f))
            (begin
              (d-WarningDialog (_ "No Denemo Directive to make conditional here.\nYou can place the \"Hide Next\"  Denemo Directive before the object you wish to omit\nand then make that directive conditional.")))))))
        
