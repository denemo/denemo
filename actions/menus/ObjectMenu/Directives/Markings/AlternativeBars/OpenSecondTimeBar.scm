;;;OpenSecondTimeBar
(let ((tag "OpenNthTimeBar")(params OpenSecondTimeBar::params))
    (if (d-Directive-standalone? tag)
        (begin
            (begin
            (d-OpenNthTimeBar "edit")))
        (begin
            (if (and (FullDurationMeasure?) (Appending?)(LastMeasure?))
                  (d-AddMeasure))
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-minpixels tag 50)
            (d-DirectivePut-standalone-postfix tag "
            \\set Score.repeatCommands = #'((volta \"2\"))
            ")
            (d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_GRAPHIC)
            (d-DirectivePut-standalone-gx  tag 25)
            (d-DirectivePut-standalone-gy  tag -34)
            (d-DirectivePut-standalone-graphic tag "SecondTimeBar")
            (d-DirectivePut-standalone-data tag (format #f "'~s" (list (cons 'volta 2))))
            (d-MoveCursorRight)
            (if (and (not (eq? params 'noninteractive)) (Confirm (_ "Start Second Time Bar") (_ "Adjust the typeset bar number to ignore second time bar(s)")))
           	 (d-SetBarNumber))
            (d-MoveCursorRight)
            (d-RefreshDisplay)
            (d-SetSaved #f))))
 
 
