;CopyFromAbove
(let ((end (d-GetEndTick)))
    (if (not end)
    	(set! end 0))
    (d-PushPosition)
    (if (d-MoveToStaffUp)
	    (begin
	    	   (while (and (< (d-GetStartTick) end) (d-MoveCursorRight) (not (Appending?))))
	    	   (if (not (Music?))
	    	   	(begin
			    (d-SetMark)
			    (while (and (d-CursorRight) (not (Music?)) (not (Appending?))))
			    (d-CursorLeft)
			    (d-Copy)
			    (d-PopPosition)
			    (d-MoveCursorRight)
			    (d-Paste))
			   (d-PopPosition)))))