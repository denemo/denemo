(let ((scale "100"))
	(set! scale		
	(d-GetUserInput "Scale Display" "Give % scaling required" "100"))
	(if (string? scale)
		(begin
			(set! scale (/ (string->number scale) 100.0))
			(format #t "Scaling by ~A~%" scale)
			(d-Zoom scale)
			(d-RefreshDisplay);;;FIXME refresh display without marking score as changed?
			)))