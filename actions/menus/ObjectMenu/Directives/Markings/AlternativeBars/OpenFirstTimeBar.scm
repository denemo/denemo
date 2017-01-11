;;;OpenFirstTimeBar
(let ((tag "OpenNthTimeBar"))
    (if (d-Directive-standalone? tag)
        (begin
            (d-OpenNthTimeBar "edit"))
        (begin
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-minpixels tag 50)
            (d-DirectivePut-standalone-postfix tag "
            \\set Score.repeatCommands = #'((volta \"1\"))
            ")
            (d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_GRAPHIC)
            (d-DirectivePut-standalone-gx  tag 25)
            (d-DirectivePut-standalone-gy  tag -34)
            (d-DirectivePut-standalone-graphic tag "FirstTimeBar")
            (d-DirectivePut-standalone-data tag (format #f "'~s" (list (cons 'volta 1))))
            (d-MoveCursorRight)
            (d-RefreshDisplay)
            (d-SetSaved #f))))
 
