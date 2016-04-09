;;;;;;; NotForLayout
 (let ((tag (d-DirectiveGetTag-standalone)) (params NotForLayout::params) (layout (d-GetLayoutName))(id (d-GetLayoutId)))
    (define (put-cond)
        (d-DirectivePut-standalone-x tag id)
        (d-DirectivePut-standalone-display tag  (string-append (_ "Not for ") layout))
        (d-DirectivePut-standalone-ty tag 60)
        (d-DirectivePut-standalone-tx tag -30))
   (define (do-rest)
    (d-PushPosition)
    (while (d-NextObject)
        (if (d-Directive-standalone? tag)
            (put-cond)))
    (d-PopPosition))
  (if tag
    (begin
        (if (pair? params)
            (begin
             (set! layout (car params))
             (set! id (cdr params))))
        (put-cond)
        
        (if  (RadioBoxMenu
                       (cons (_ "Apply condition to all further cases in this staff")   'yes)   
                        (cons (_ "Just for this one") #f))
                     (begin
                            (do-rest)
                            (d-InfoDialog (string-append (_ "Standalone Directives ") "\"" tag "\"" (_ "  in this staff from the cursor onwards will not be typeset for the layout ") "\"" layout "\"" )))
                     (d-InfoDialog (string-append (_ "This Directive ") "\"" tag "\"" (_ " will not be typeset for the layout ") "\"" layout"\"")))
        (d-SetSaved #f)
        (d-RefreshDisplay))
    (begin
          (if (Music?)
            (d-DirectiveNotForLayout #f)
            (d-WarningDialog (_ "The cursor is not on a Denemo Directive.\nYou can place the \"Void\" Denemo Directive before the object you wish to omit\nand then make that directive conditional."))))))
