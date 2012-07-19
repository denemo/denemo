;;;LimitInterSystemSpace
(define (LimitInterSystemSpace::set value)
	(d-DirectivePut-paper-postfix "LimitInterSystemSpace" (string-append "
page-limit-inter-system-space = ##t
page-limit-inter-system-space-factor = " value)))

(if LimitInterSystemSpace::params
	(begin
		(LimitInterSystemSpace::set (number->string LimitInterSystemSpace::params)))
	(if (d-Directive-paper? "LimitInterSystemSpace")
		(begin
			(d-DirectiveDelete-paper "LimitInterSystemSpace")
			(d-InfoDialog "Limit on space between systems removed"))
		(let ((value
		(d-GetUserInput "Spacing Between Systems" "Give spacing limit (1=no extra space)" "1.2")))
		(disp "value is " value "\n")
		(if (and (string? value) (string->number value))
		 (LimitInterSystemSpace::set  value)))))		
(d-SetSaved #f) 
