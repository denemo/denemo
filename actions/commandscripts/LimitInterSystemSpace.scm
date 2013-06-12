;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;LimitInterSystemSpace
(define (LimitInterSystemSpace::set value)
	(d-DirectivePut-paper-postfix "LimitInterSystemSpace" (string-append "
page-limit-inter-system-space = ##t
page-limit-inter-system-space-factor = " value)))
(if (equal? LimitInterSystemSpace::params "edit")
	(begin
		(set! LimitInterSystemSpace::params #f)
		(d-DirectiveDelete-paper "LimitInterSystemSpace")))
(if LimitInterSystemSpace::params
	(begin
		(LimitInterSystemSpace::set (number->string LimitInterSystemSpace::params)))
	(if (d-Directive-paper? "LimitInterSystemSpace")
		(begin
			(d-DirectiveDelete-paper "LimitInterSystemSpace")
			(d-InfoDialog (_ "Limit on space between systems removed")))
		(let ((value
		(d-GetUserInput (_ "Spacing Between Systems") (_ "Give spacing limit (1=no extra space)") "1.2")))
		(disp "value is " value "\n")
		(if (and (string? value) (string->number value))
		 (LimitInterSystemSpace::set  value)))))		
(d-SetSaved #f) 
