;;SetFontSize
(let* ((tag "SetFontSize") (newsize (d-DirectiveGet-score-data tag)) (size  (d-ScoreProperties "query=fontsize")))
	(if (not newsize)
		(set! newsize size))
	(set! newsize  (d-GetUserInput "Overall Score Sizing"  "Give font size to use" newsize))
	(if newsize
		(begin
			(d-DirectivePut-score-data tag newsize)
			(d-DirectivePut-score-postfix tag 
				(string-append
				"#(set-global-staff-size " newsize ")\n" ))
			(d-SetSaved #f))))