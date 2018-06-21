;;AllowBreakAtBeat
(if (even? ((duration::GetNumerator)))
        (let* ((ticks (/ (GetMeasureTicks) 2)) (ticks2 (/ ticks 2)) )
		(while (d-NextNote)
		    (if (or (eq? (d-GetStartTick) ticks2) (eq? (d-GetStartTick) ticks))
		        (begin 
		            (d-AllowLineBreak 'non-interactive)
		            (d-MoveCursorRight)))))
	(let    ((ticks (/ (GetMeasureTicks) 3)))      
		  (while (d-NextNote)
			    (if (eq? (d-GetStartTick) ticks)
				(begin 
				    (d-AllowLineBreak 'non-interactive)
				    (d-MoveCursorRight))))))