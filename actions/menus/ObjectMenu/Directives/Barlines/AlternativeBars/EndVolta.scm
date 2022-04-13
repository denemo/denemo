;;;EndVolta
(let ((tag "EndVolta"))
    (if (d-Directive-standalone? tag)
        (let  ((choice (d-GetOption  (string-append (_ "Help") stop (_ "Delete") stop (_ "Advanced") stop))))
           (if choice
            (if (equal? choice (_ "Delete"))
                (d-DirectiveDelete-standalone tag)
                (if (equal? choice (_ "Help"))
                    (d-InfoDialog (_ "This closes a 1st, 2nd, ... n'th time bar"))
                    (if (not (d-DirectiveTextEdit-standalone tag))
                        (d-DirectiveDelete-standalone tag))))))
           (begin
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-minpixels  tag 50)
            (d-DirectivePut-standalone-postfix tag "
    \\set Score.repeatCommands = #'((volta #f))
    ")
            (d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_GRAPHIC)
            (d-DirectivePut-standalone-gx  tag 18)
            (d-DirectivePut-standalone-gy  tag -36)
            (d-DirectivePut-standalone-graphic tag "EndSecondTimeBar")
            (d-MoveCursorRight)))
            (d-SetSaved #f)
            (d-RefreshDisplay))
     
