;;;OpenFirstTimeBar
(let ((tag "Volta1"))
(d-Directive-standalone tag)
(d-DirectivePut-standalone-minpixels  tag 50)
(d-DirectivePut-standalone-postfix tag "
\\set Score.repeatCommands = #'((volta \"1\"))
")
(d-DirectivePut-standalone-gx  tag 37)
(d-DirectivePut-standalone-gy  tag -34)
(d-DirectivePut-standalone-graphic tag "FirstTimeBar")
(d-MoveCursorRight)
(d-RefreshDisplay)
(d-SetSaved #f))
 