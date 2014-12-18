;;DynamicsExtraSpace
(let ((tag "DynamicExtraSpace") (amount #f))
(set! amount (d-GetUserInput (_ "Dynamics Spacing") (_ "Give extra spacing required") "20"))
(if amount
	(StandAloneDirectiveProto (cons tag  (string-append "\\override DynamicText.extra-spacing-width = #'(-" amount " . " amount ") "))
			 #t "\nS\nDenemo" )))
