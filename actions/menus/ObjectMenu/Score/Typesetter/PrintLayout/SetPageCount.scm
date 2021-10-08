;;;SetPageCount
(let ((tag "SetPageCount")(count (d-GetUserInput (_ "Total Page Count") (_ "Give Pages Required\n(0 for optimal): ") "4")))
	(if (and count (string->number count) (> (string->number count) 0))
		(begin
			(d-DirectivePut-score-prefix tag (string-append "\\paper { page-count=" count "}"))
			(d-DirectivePut-score-display tag (string-append (_ "Page Count") count))
			
			(SetDirectiveConditional "score"  tag)
			(d-SetSaved #f))
		(begin
			(d-DirectiveDelete-score tag)
			(d-WarningDialog (_ "Optimal page count restored")))))
