;;;InsertOneNote
(if (and (Appending?) (d-GetNonprinting))
	(eval-string (string-append "(d-" (string-upcase (d-GetCursorNote)) ")"))
	(eval-string (string-append "(d-Insert" (number->string (d-GetPrevailingDuration)) ")")))