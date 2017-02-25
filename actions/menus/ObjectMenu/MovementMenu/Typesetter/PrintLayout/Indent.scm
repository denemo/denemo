;;;;;; Indent
(let ((tag "Indent")(amount "0.0") (current "0.0")(thematch #f))
  (set! current (d-DirectiveGet-layout-data tag))
  (if (not current)
    (set! current "15.0"))
  (set! amount (d-GetUserInput (_ "Choose indent for Current Movement") (_ "Give indent or Cancel to unset movement indent") current))
  (if (and amount (string->number amount))
  	(begin
		  (d-DirectivePut-layout-data tag amount)
		  (d-DirectivePut-layout-postfix tag (string-append "indent = " amount "\n"))
		  (d-DirectivePut-layout-display tag (string-append (_ "Indent= ") amount)))
	(begin
		(d-DirectiveDelete-layout tag)
		(if (not Indent::params)
			(d-WarningDialog (_ "Movement indent dropped. Score indent will be used for this movement")))))
  (d-SetSaved #f))
  
