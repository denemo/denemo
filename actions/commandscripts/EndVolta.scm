;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;EndVolta
(if (d-Directive-standalone? "EndVolta")
	(let  ((choice (d-GetOption  (string-append (_ "Delete") stop (_ "Advanced") stop))))
	(if (equal? choice (_ "Delete"))
		(d-DirectiveDelete-standalone "EndVolta")
		(d-DirectiveTextEdit-standalone "EndVolta")))
	(begin
(d-Directive-standalone "EndVolta")
(d-DirectivePut-standalone-minpixels  "EndVolta" 50)
(d-DirectivePut-standalone-postfix "EndVolta" "
\\set Score.repeatCommands = #'((volta #f))
")
(d-DirectivePut-standalone-gx  "EndVolta" 18)
(d-DirectivePut-standalone-gy  "EndVolta" -36)
(d-DirectivePut-standalone-graphic "EndVolta" "EndSecondTimeBar")
(d-MoveCursorRight)))
(d-SetSaved #f)
(d-RefreshDisplay)
 