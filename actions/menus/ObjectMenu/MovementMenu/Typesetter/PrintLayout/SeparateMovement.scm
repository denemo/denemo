;;;SeparateMovement
(let ((tag "SeparateMovement"))
	(if (d-Directive-scoreheader? "BookTitle")
		(d-WarningDialog (_ "Cannot use this command with Book Titles"))
		(begin
			(if (d-Directive-movementcontrol? tag)
				(begin
					(d-DirectiveDelete-movementcontrol tag)
					(d-InfoDialog (_ "Separated Movement status dropped")))
				(begin 
					(d-DirectivePut-movementcontrol-prefix tag "\\bookpart {\n%start of bookpart\n")
					(d-DirectivePut-movementcontrol-postfix tag "\n}\n%end of bookpart\n")
					(d-InfoDialog (_ "Movement will be separated from others"))
					(d-SetSaved #f))))))