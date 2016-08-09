;;StaffSpacer
(let ((tag "StaffSpacer")(data #f) (direction "^")(value "10")(raise "raise"))
 (if (d-Directive-standalone? tag)
    (set! data (d-DirectiveGet-standalone-data tag)))
  (if data 
        (set! data (eval-string data)))
  (if data
    (begin
        (set! direction (car data))
        (set! value (cdr data))))
  
    (set! value (d-GetUserInput (_ "Spacing for Staff") (_ "Give value for extra space: ") value))
    (if value
        (begin
            (set! direction (RadioBoxMenu
                             (cons (_ "Space Above Staff")   "^")   
                                (cons (_ "Space Below Staff")  "_")))
            
             (if (equal? direction "_")
                (set! raise "lower"))
            (StandAloneDirectiveProto (cons tag (string-append "<>" direction "\\markup { \\" raise " #" value " \" \" }")) #f "\n⬍\nDenemo\n20")
            (d-DirectivePut-standalone-gy tag -44)
            (d-DirectivePut-standalone-data tag (string-append "(cons " "\"" direction "\" \"" value "\")"))
            (d-MoveCursorRight)
        (d-RefreshDisplay)
        (d-SetSaved #f))))
        
