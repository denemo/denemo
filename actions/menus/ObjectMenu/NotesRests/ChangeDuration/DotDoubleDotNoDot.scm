;DotDoubleDotNoDot
(let ((dots (d-GetDots)))
	(if dots
		(cond ((or (= dots 0) (= dots 1))
				(d-AddDot))
			   ((= dots 1)
			   	(d-RemoveDot))
			   (else
			   	(d-RemoveDot)
			   	(d-RemoveDot)))
		(d-InfoDialog (_ "Cursor must be on a chord/note/rest"))))
