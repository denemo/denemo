(let ((num (number->string (1- (d-GetMeasure)))))
(define result (d-GetUserInput (_ "Change Barnumber in typeset score") (_ "Please enter a number to set the current bar number. The typeset score will continue to count up from there.") num))
	(if result 
	(let ((barnumberstring (string-append "\\set Score.currentBarNumber = #" result " " ) ))
		(d-Directive-standalone "BarNumber")
		(d-DirectivePut-standalone-display "BarNumber" (string-append (_ "BarNumber = ") result))
		(d-DirectivePut-standalone-minpixels  "BarNumber"  30)
		(d-SetMeasureNumberOffset (- (string->number result) (d-GetMeasure)))
		(d-DirectivePut-standalone-postfix "BarNumber"  barnumberstring )
		(d-RefreshDisplay)
	)
	#f  ; cancel
	)
)
