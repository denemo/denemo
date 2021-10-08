;;;;;;;; ForAllLayouts
 (let ((tag (d-DirectiveGetTag-standalone))(params ForAllLayouts::params))
   (define (d-InfoDialog string)
        (Help::TimedNotice (string-append string "\n") 5000))
  (if tag
    (begin
        (d-DirectivePut-standalone-allow tag 0)
        (d-DirectivePut-standalone-ignore tag 0)
        (d-InfoDialog (string-append (_ "This Directive ") "\"" tag "\"" (_ " will be typeset for all layouts ")))
        
        (d-SetSaved #f)
        (d-RefreshDisplay))
    (d-MakeDirectiveConditional)))

