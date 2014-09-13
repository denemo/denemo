;;;TimeSignature
(let ((tag "TimeSignature")(numerator #f)(denominator #f))
	(d-InsertTimeSig)
	(d-MoveCursorLeft)
	(if (Timesignature?)
	(begin
		(set! numerator (car (string-split (d-GetPrevailingTimesig) #\/)))
		(set! denominator (cadr (string-split (d-GetPrevailingTimesig) #\/)))
		(d-DirectivePut-timesig-postfix tag (string-append "<>-\\tweak #'extra-offset #'(-5 . -3) -\\tweak baseline-skip #2   "  "^\\markup\\scale #'(2 . 2)\\column{\\line\\large{\\bold "numerator "}\\line\\large{\\bold " denominator "}}"))
		(d-SetSaved #f)
		(d-RefreshDisplay))))  