;;; d-SetFontSize
(define SetScoreSize::Size (d-ScoreProperties "query=fontsize"))	
(set! SetScoreSize::Size  (d-GetUserInput "Overall Score Sizing"  "Give font size to use" SetScoreSize::Size))
(if (boolean? SetScoreSize::Size)		
	(set! SetScoreSize::Size (d-ScoreProperties "query=fontsize")))						       
(d-ScoreProperties (string-append "fontsize="   SetScoreSize::Size))		
(d-RefreshDisplay)