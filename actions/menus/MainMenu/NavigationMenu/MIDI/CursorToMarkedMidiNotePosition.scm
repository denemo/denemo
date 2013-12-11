;;;CursorToMarkedMidiNotePosition
(let ((marked (d-GetMarkedMidiNoteSeconds)))
	(if marked
		(begin
			;(while (d-MoveToStaffUp))
			(d-MoveToBeginning)
			(let loop ()
				(define start (d-GetMidiOnTime))
				(define end (d-GetMidiOffTime))
				(if (and start end (>= marked start) (<= marked end))
					#t
					(if (d-NextObject)
						(loop)
						#f))))))
		
