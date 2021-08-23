;;SetFontSize
(let* ((tag "SetFontSize") (newsize (d-DirectiveGet-paper-data tag)) (size  (d-ScoreProperties "query=fontsize")))
	(if (not newsize)
		(set! newsize size))
	(set! newsize  (d-GetUserInput "Overall Score Sizing"  "Give font size to use" newsize))
	(if newsize
		(begin
			(d-DirectivePut-paper-data tag newsize)
			(d-DirectivePut-paper-postfix tag 
				(string-append
				"\n} #(set-global-staff-size " newsize ") \\paper {\n" )) ;this cheats, placing the setting *outside* the paper block. It seems we don't have a score-nfix override setting to place stuff here. FIXME
			(d-SetSaved #f))))
