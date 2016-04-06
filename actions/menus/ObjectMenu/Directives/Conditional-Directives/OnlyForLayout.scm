;;;;;;;; OnlyForLayout
 (let ((tag (d-DirectiveGetTag-standalone)) ( id (d-GetLayoutId)) (text #f) (name (d-GetLayoutName)))
    (define (do-rest)
    (d-PushPosition)
    (while (d-NextObject)
        (if (d-Directive-standalone? tag)
            (d-DirectivePut-standalone-y tag id)))
    (d-PopPosition))
  (if tag
    (let () (disp "params are " OnlyForLayout::params " ok")
        (if OnlyForLayout::params
            (begin
                (set! id (cdr OnlyForLayout::params))
                (set! name (car OnlyForLayout::params))))
        (set! text (string-append (_ "for ") name))
        (d-DirectivePut-standalone-display tag text)
        (d-DirectivePut-standalone-ty tag 60)
        (d-DirectivePut-standalone-tx tag -30)
        (d-DirectivePut-standalone-y tag id)
        (if  (RadioBoxMenu
                       (cons (_ "Apply condition to all further cases in this staff")   'yes)   
                        (cons (_ "Just for this one") #f))
                     (begin
                            (do-rest)
                            (d-InfoDialog (string-append (_ "Standalone Directives ") "\"" tag "\"" (_ "  in this staff from the cursor onwards will only be typeset for the layout ") "\"" (car layout) "\"" )))
                     (d-InfoDialog (string-append (_ "This Directive ") "\"" tag "\"" (_ " will only be typeset for the layout ") "\"" (car layout) "\"")))
        (d-SetSaved #f)
        (d-RefreshDisplay))
    (begin
      (if (Music?)
            (d-DirectiveOnlyForLayout #f)
        (d-WarningDialog (_ "The cursor is not on a Denemo Directive.\nYou can place the \"Hide Next\"  Denemo Directive before the object you wish to omit\nand then make that directive conditional.")))))) 
        
