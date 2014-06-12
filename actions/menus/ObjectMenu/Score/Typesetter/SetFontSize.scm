;;; d-SetFontSize
(let ((size (d-ScoreProperties "query=fontsize")) (newsize  SetFontSize::params))
	(if (not newsize)
		(set! newsize  (d-GetUserInput "Overall Score Sizing"  "Give font size to use" size)))
(if (and newsize (not (equal? size newsize)))					       
	(begin
		(d-ScoreProperties (string-append "fontsize="   newsize))		
		(d-SetSaved #f))))
