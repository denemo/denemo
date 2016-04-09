;;;;;;;; ForAllLayouts
 (let ((tag (d-DirectiveGetTag-standalone)) )
  (define (do-rest)
    (d-PushPosition)
    (while (d-NextObject)
        (if (d-Directive-standalone? tag)
             (begin  (d-DirectivePut-standalone-display tag "")(d-DirectivePut-standalone-x tag 0)(d-DirectivePut-standalone-y tag 0))))
    (d-PopPosition))
  (if tag
    (begin
        (d-DirectivePut-standalone-display tag "")
        (d-DirectivePut-standalone-x tag 0)
        (d-DirectivePut-standalone-y tag 0)
        (if  (RadioBoxMenu
                       (cons (_ "Apply condition to all further cases in this staff")   'yes)   
                       (cons (_ "Just for this one") #f))
            (begin
                (do-rest)              
                (d-InfoDialog (string-append (_ "Standalone Directives ") "\"" tag "\"" (_ " in this staff from the cursor onwards will be typeset for all layouts "))))
            (d-InfoDialog (string-append (_ "This Directive ") "\"" tag "\"" (_ " will be typeset for all layouts "))))
        
        (d-SetSaved #f)
        (d-RefreshDisplay))
    (begin
        (if (Music?)
            (d-DirectiveForAllLayouts #f)
            (d-WarningDialog (_ "The cursor is not on a Denemo Directive.\nYou can place the \"Void\" Denemo Directive before the object you wish to omit\nand then make that directive conditional.")))))) 
