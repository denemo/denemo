;;;SetSystemCount
(let ((tag "SetSystemCount")(count (d-GetUserInput (_ "Total Systems Count") (_ "Give Systems Required\n(0 for optimal): ") "4")))
	(if (and count (string->number count) (> (string->number count) 0))
		(begin
			(d-DirectivePut-score-prefix tag (string-append "\\paper { system-count=" count "}"))
			(d-DirectivePut-score-display tag (string-append (_ "System Count") count))
			(SetDirectiveConditional "score"  tag)
			(d-SetSaved #f))
		(begin
			(d-DirectiveDelete-score tag)
			(d-WarningDialog (_ "Optimal system count restored")))))
