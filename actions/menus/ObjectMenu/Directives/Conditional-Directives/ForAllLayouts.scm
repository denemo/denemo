;;;;;;;; ForAllLayouts
 (let ((tag (d-DirectiveGetTag-standalone)) )
  (if tag
    (begin
        (d-DirectivePut-standalone-display tag "")
        (d-DirectivePut-standalone-x tag 0)
        (d-DirectivePut-standalone-y tag 0)
        (d-SetSaved #f)
        (d-RefreshDisplay))
    (begin
        (if (Music?)
            (d-DirectiveForAllLayouts #f)
            (d-WarningDialog (_ "The cursor is not on a Denemo Directive.\nYou can place the \"Void\" Denemo Directive before the object you wish to omit\nand then make that directive conditional.")))))) 
