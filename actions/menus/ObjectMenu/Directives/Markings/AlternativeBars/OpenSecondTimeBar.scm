;;;OpenSecondTimeBar
(let ((tag "OpenNthTimeBar"))
    (if (d-Directive-standalone? tag)
        (begin
            (begin
            (d-OpenNthTimeBar "edit")))
        (begin
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-minpixels tag 50)
            (d-DirectivePut-standalone-postfix tag "
            \\set Score.repeatCommands = #'((volta \"2\"))
            ")
            (d-DirectivePut-standalone-gx  tag 37)
            (d-DirectivePut-standalone-gy  tag -34)
            (d-DirectivePut-standalone-graphic tag "SecondTimeBar")
            (d-DirectivePut-standalone-data tag (format #f "'~s" (list (cons 'volta 2))))
            (d-MoveCursorRight)
            (d-RefreshDisplay)
            (d-SetSaved #f))))
 
 
