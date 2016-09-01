(let ((tag "SetPageCount")(count (d-GetUserInput (_ "Total Page Count") (_ "Give Pages Required: ") "4")))
	(if (and count (string->number count))
		(begin
			(d-DirectivePut-score-prefix tag (string-append "\\paper { page-count=" count "}"))
			(d-DirectivePut-score-display tag (string-append (_ "Page Count") count))
			
			(SetDirectiveConditional #f (cons "score"  tag))
			(d-SetSaved #f))
		(d-WarningDialog (_ "Cancelled"))))
