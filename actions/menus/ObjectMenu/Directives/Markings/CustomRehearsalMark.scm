;;; CustomRehearsalMark
(let ((user-input CustomRehearsalMark::params) (tag "CustomRehearsalMark"))
    
    (if (and (not user-input) (d-Directive-standalone? tag))
        (set! user_input (d-DirectiveGet-standalone-data tag)))
    (if (not user-input)
        (set! user-input "X"))
    (set! user-input (d-GetUserInputWithSnippets (_ "Custom Rehearsal/Text Mark") (_ "Give text to use for mark") user-input))
    (if user-input   ;in case the user pressed Escape do nothing    
        (let ((text (car user-input))(data (cdr user-input))(position (RadioBoxMenu (cons (_ "left") "left") (cons (_ "center") "center") (cons (_ "right") "right")))) 
          (d-Directive-standalone tag)
          (d-DirectivePut-standalone-display tag text)
          (d-DirectivePut-standalone-postfix tag  (string-append  " \\once \\override Score.RehearsalMark #'self-alignment-X = #" position " \\mark \\markup {" text "}" ) )
          (d-DirectivePut-standalone-grob  tag  "RehearsalMark")
          (d-DirectivePut-standalone-minpixels  tag  10)
          (d-DirectivePut-standalone-data tag data)
          (d-SetSaved #f)
          (d-RefreshDisplay))))

