;;;OpenFirstTimeBar
(let ((tag "OpenFirstTimeBar"))
	(if (d-Directive-standalone? tag)
		(begin
			(d-WarningDialog (_ "This starts a first time bar. To change the text use the Nth time bar command.")))
		(begin
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
			(d-SetSaved #f))))
 