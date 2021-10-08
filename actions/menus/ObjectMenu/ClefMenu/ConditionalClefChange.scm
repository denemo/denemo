;ConditionalClefChange
(let ((tag "ConditionalClefChange"))
(d-InsertClef)
(d-MoveCursorLeft)
(if (Clef?)
	(begin
		(d-DirectivePut-clef-prefix tag "%{ ")
		(d-DirectivePut-clef-postfix tag " %} ")
		(d-DirectivePut-clef-graphic tag "\n?\nDenemo\n24")
		(d-DirectivePut-clef-gy tag 80)
		(SetDirectiveConditional "clef" tag)
		(d-SetSaved #f))))
