;;EvenOutStaffLengths
(d-PushPosition)
(d-GoToPosition #f 1 1 1)
(let ((maxmeasures (d-GetMeasuresInStaff)) (uneven #f))
	(let loop ()
		(define measures (d-GetMeasuresInStaff))
		(if (not (= maxmeasures measures))
			(begin
			(set! uneven measures)
			(if (> measures maxmeasures)
			  (set! maxmeasures measures))))
		(if (d-MoveToStaffDown)
			(loop)))
	(d-GoToPosition #f 1 1 1)
	(if uneven
	  (begin
	    (let loop ()
	      (define needed (- maxmeasures (d-GetMeasuresInStaff)))	      
	      (let addloop ((num needed))
		  (if (positive? num)
		      (begin
			(d-MoveToEnd)
			(d-SetSaved #f)
			(d-InsertMeasureAfter)
			(addloop (- num 1)))))
		  (if (d-MoveToStaffDown)
		    (loop))))))
(d-PopPosition)
;;;;;;
