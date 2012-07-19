(let ()
(define result (d-GetUserInput "Change Barnumber in printout" "Please enter a number to set the current bar number. The printout will continue to count up from there." "1"))
	(if result 
	(let ((barnumberstring (string-append "\\set Score.currentBarNumber = #" result ) ))
		(d-Directive-standalone "BarNumber")
		(d-DirectivePut-standalone-display "BarNumber" (string-append "BarNumber = " result))
		(d-DirectivePut-standalone-minpixels  "BarNumber"  30)
		(d-DirectivePut-standalone-postfix "BarNumber"  barnumberstring )
		(d-RefreshDisplay)
	)
	#f  ; cancel
	)
)