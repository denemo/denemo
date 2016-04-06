;;;;;;; NotForLayout
 (let ((tag (d-DirectiveGetTag-standalone)) ( id (d-GetLayoutId)))
   (define (do-rest)
    (d-PushPosition)
    (while (d-NextObject)
        (if (d-Directive-standalone? tag)
            (d-DirectivePut-standalone-x tag id)))
    (d-PopPosition))
  (if tag
    (let ()
        (define text (string-append (_ "Not for ") (d-GetLayoutName)))
        (d-DirectivePut-standalone-display tag text)
        (d-DirectivePut-standalone-ty tag 60)
        (d-DirectivePut-standalone-tx tag -30)
        (d-DirectivePut-standalone-x tag id)
        
         (if  (RadioBoxMenu
                       (cons (_ "Apply condition to all further cases in this staff")   'yes)   
                        (cons (_ "Just for this one") #f))
                     (begin
                            (do-rest)
                            (d-InfoDialog (string-append (_ "Standalone Directives ") "\"" tag "\"" (_ "  in this staff from the cursor onwards will not be typeset for the layout ") "\"" (car layout) "\"" )))
                     (d-InfoDialog (string-append (_ "This Directive ") "\"" tag "\"" (_ " will not be typeset for the layout ") "\"" (car layout) "\"")))

        (d-SetSaved #f)
        (d-RefreshDisplay))
    (begin
          (if (Music?)
            (d-DirectiveNotForLayout #f)
            (d-WarningDialog (_ "The cursor is not on a Denemo Directive.\nYou can place the \"Void\" Denemo Directive before the object you wish to omit\nand then make that directive conditional."))))))
