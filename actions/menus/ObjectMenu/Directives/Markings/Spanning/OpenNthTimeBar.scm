;;;OpenNthTimeBar
(let ((tag "Volta1") (nth #f))
    (set! nth OpenNthTimeBar::params)
    (if (not nth)
        (set! nth (d-GetUserInput (_ "Nth Time Bar") (_ "Give text for the open nth time bar") "1,2")))
    (if nth
        (begin
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-minpixels  tag 50)
            (d-DirectivePut-standalone-postfix tag (string-append "
            \\set Score.repeatCommands = #'((volta \"" nth "\"))
            "))
            (d-DirectivePut-standalone-gx  tag 37)
            (d-DirectivePut-standalone-gy  tag -34)
            (d-DirectivePut-standalone-graphic tag "NthTimeBar")
            (d-MoveCursorRight)
            (d-RefreshDisplay)
            (d-SetSaved #f))))
 
