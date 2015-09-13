;;;ScoreChooseNotehead
(let ((tag "ScoreChooseNotehead")
    (params ChooseNotehead::params))
    	(if (equal? params "edit")
        	(set! params #f))
	(d-ChooseNotehead 'score))
