;;AllowBreakAtHalfBar   
        (let ((ticks (/ (GetMeasureTicks) 2)))
        (while (d-NextNote)
            (if (eq? (d-GetStartTick) ticks)
                (begin 
                    (d-AllowLineBreak 'non-interactive)
                    (d-MoveCursorRight)))))