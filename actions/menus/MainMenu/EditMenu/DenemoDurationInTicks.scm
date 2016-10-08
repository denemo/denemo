(let ((tag "DenemoDurationInTicks")(params DenemoDurationInTicks::params))
	(if (equal? params "edit")
		(set! params #f))
	(if (not params)
	        (begin
	        	(set! params (d-GetUserInput (_ "Set Denemo Duration") (_ "Give duration desired in ticks (PPQN) ") (if (d-GetDurationInTicks) (number->string (d-GetDurationInTicks)) "0")))))
         (if params
			(d-SetDurationInTicks (string->number params)))
	(d-SetSaved #f)
	(d-RefreshDisplay))