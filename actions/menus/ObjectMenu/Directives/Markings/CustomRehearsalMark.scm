;;; CustomRehearsalMark
(let ((value CustomRehearsalMark::params) (tag "CustomRehearsalMark"))
    (if (equal? value "edit")
        (set! value #f))
    (if (and (not value) (d-Directive-standalone? tag))
        (begin
            (set! value (d-DirectiveGet-standalone-data tag))
            (if value 
                (begin
                    (set! value (eval-string value))
                    (set! value (assq-ref value 'value))))))
    (if (not value)
        (set! value "X"))
    (set! value (d-GetUserInputWithSnippets (_ "Custom Rehearsal/Text Mark") (_ "Give text to use for mark") value))
    (if value   ;in case the user pressed Escape do nothing    
        (let ((text (cdr value))(data (car value))(position (RadioBoxMenu (cons (_ "left") "left") (cons (_ "center") "center") (cons (_ "right") "right")))) 
          (d-Directive-standalone tag)
          (d-DirectivePut-standalone-display tag data)
          (d-DirectivePut-standalone-graphic tag "RehearsalMark")
          (d-DirectivePut-standalone-gx tag 15)
          (d-DirectivePut-standalone-postfix tag  (string-append  " \\once \\override Score.RehearsalMark #'self-alignment-X = #" position " \\mark \\markup {" text "}" ) )
          (d-DirectivePut-standalone-grob  tag  "RehearsalMark")
          (d-DirectivePut-standalone-minpixels  tag  30)
          (d-DirectivePut-standalone-data tag (string-append "'(('value . \"" data "\"))"))
          (d-SetSaved #f)
          (d-RefreshDisplay))))

