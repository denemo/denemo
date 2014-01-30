;;;;;;;; OnlyForLayout
 (let ((tag (d-DirectiveGetTag-standalone)) ( id (d-GetLayoutId)) (text #f) (name (d-GetLayoutName)))
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
        (d-SetSaved #f)
        (d-RefreshDisplay))
    (begin
        (d-WarningDialog (_ "The cursor is not on a Denemo Directive.\nYou can place the \"Void\"  Denemo Directive before the object you wish to omit\nand then make that directive conditional."))))) 
        
