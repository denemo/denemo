;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ScoreTitle
(let ((tag "ScoreTitle") (title ScoreTitle::params))
	(define (blank-title)
			(if (not (d-DirectiveGet-header-postfix tag))
				(begin 
					(d-DirectivePut-header-override tag 0) 
					(d-DirectivePut-header-postfix tag "title = \"\""))))
	(SetScoreHeaderField "title" title)
	(DenemoPrintAllHeaders)		
	(d-PushPosition)
	(blank-title)
	(while (d-NextMovement)
		(blank-title))
	(d-PopPosition))
		