;;SetFontSize
(let ((tag "SetFontSize") (params SetFontSize::params)(newsize #f) (size  (d-ScoreProperties "query=fontsize")))
	(if params (if (and (string? params)  (string->number params))
					(set! newsize params)
					(if (number? params)
						(set! newsize (number->string params)))))
	(if (not newsize)
		(begin
			(set! newsize (d-DirectiveGet-score-data tag))
			(if (not newsize)
				(begin
					(set! newsize (d-DirectiveGet-paper-data tag));;legacy code
					(d-DirectiveDelete-paper tag)))
			(if (not newsize)
				(set! newsize size))					
			(set! newsize  (d-GetUserInput (_ "Overall Score Sizing")  (_ "Give font size to use") newsize))))
	(if newsize
		(begin
			(d-DirectivePut-score-data tag newsize)
			(d-DirectivePut-score-override tag DENEMO_ALT_OVERRIDE)
			(d-DirectivePut-score-prefix tag 
				(string-append
				"#(set-global-staff-size " newsize ") \n" ))
			(d-SetSaved #f))))
