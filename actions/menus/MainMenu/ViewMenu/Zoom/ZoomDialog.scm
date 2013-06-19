(let ((scale "100"))
	(set! scale		
	(d-GetUserInput (_ "Scale Display") (_ "Give % scaling required") "100"))
	(if (string? scale)
		(begin
			(set! scale (/ (string->number scale) 100.0))
			(format #t "Scaling by ~A~%" scale)
			(d-Zoom scale)
			(d-RefreshDisplay)
			)))