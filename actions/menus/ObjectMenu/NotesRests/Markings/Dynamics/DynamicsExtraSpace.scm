;;DynamicsExtraSpace
(let ((tag "DynamicsExtraSpace") (amount #f))
(set! amount (d-DirectiveGet-standalone-data tag))
(set! amount (d-GetUserInput (_ "Dynamics Spacing") (_ "Give extra spacing required") (if amount amount "10")      ))
(if (and (string? amount) (string->number amount))
	(begin
		(StandAloneDirectiveProto (cons tag  (string-append "\\override DynamicText.extra-spacing-width = #'(-" amount " . " amount ") "))
			 #f "\nS\nDenemo" )
		(d-DirectivePut-standalone-data tag amount)
		(d-SetSaved #f))))
