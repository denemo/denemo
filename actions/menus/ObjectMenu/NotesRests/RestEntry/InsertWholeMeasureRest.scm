;;;;InsertWholeMeasureRest
(let ((duration (d-GetPrevailingDuration)))
	(if (or (None?)  (zero? (d-GetDurationInTicks)))
	    (begin
		(DenemoWholeMeasureRestCommand)
		(d-MoveCursorRight)
		(d-GetPrevailingDuration duration))
	    (begin  
		(if (d-MoveToMeasureRight)
		     (if (or (None?)  (zero? (d-GetDurationInTicks)))
		        (DenemoWholeMeasureRestCommand)
		        (begin
		        	(d-MoveToMeasureLeft)
		        	 (let loop () 
		        		(if (d-Directive-chord? DenemoWholeMeasureRestTag)
		        			(DenemoWholeMeasureRestCommand)
		        			(if (d-NextObjectInMeasure) 
		        				(loop))))))
		    (begin
		        (d-AppendMeasureAllStaffs)  
		        (d-MoveCursorRight)
		        (DenemoWholeMeasureRestCommand)
		        (d-GetPrevailingDuration duration)
		        (d-MoveCursorRight))))))
