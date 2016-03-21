;;LengthenHairpins
(let ((tag "LengthenHairpins") (amount #f))
(set! amount (d-DirectiveGet-standalone-data tag))
(set! amount (d-GetUserInput (_ "Hairpin Length") (_ "Give extra length required") (if amount amount "10")      ))
(if (and (string? amount) (string->number amount))
	(begin
		(StandAloneDirectiveProto (cons tag  (string-append "\\once \\override Hairpin.minimum-length = #" amount " "))
			 #f "\nH\nDenemo" )
		(d-DirectivePut-standalone-data tag amount)
		(d-SetSaved #f))))
