;;;SetScoreSizeForLayout
(let* ((tag (string-append "FontSize:" (d-GetLayoutName)))(size  (d-DirectiveGet-score-data tag)))
	(if (not (d-Directive-score? "FontSize"))
		(d-SetFontSize (d-ScoreProperties "query=fontsize")))
	(if (not size)
		(set! size  (d-ScoreProperties "query=fontsize")))
	(set! size  (d-GetUserInput (_ "Score Size")  (_ "Give font size to use") size))
	(if size
		(begin
			(d-DirectivePut-score-data tag size)
			(d-DirectivePut-score-override tag DENEMO_ALT_OVERRIDE)
			(d-DirectivePut-score-prefix tag 
				(string-append
				"#(set-global-staff-size " size ") \n" ))
			(d-DirectivePut-score-allow tag (d-GetLayoutId))
			(d-SetSaved #f))
		(d-WarningDialog (_ "Cancelled"))))