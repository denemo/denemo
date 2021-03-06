;;;ConvertToWholeMeasureRests
;This version copes with multiple rests equal to a whole measure rest as long as they are not broken up (e.g. by a barline)
(d-PushPosition)
(let ()
 (define (do-one)
	(if (and (Rest?) (not (d-Directive-chord? "WholeMeasureRest")))
       			(let ((measuredur (duration::GetWholeMeasureInTicks))
       				(this (d-GetDurationInTicks)))
                    (d-SetMark)
                    (while (and (d-CursorRight) (Rest?))
                        (set! this (+ this (d-GetDurationInTicks))))
                    (if (not (Rest?))
                        (d-CursorLeft))
		            (if (eq? this measuredur)
		                (begin
		                    (d-Cut)
                            (if (None?)
                                (d-WholeMeasureRest)
                                (begin
                                    (while (and (d-CursorRight) (not (Appending?))))
                                    (d-Cut)
                                    (d-WholeMeasureRest)
                                    (d-Paste))))))))
  (define (do-staff)
      (d-MoveToBeginning)
      (do-one)
       (while (d-NextObject)
       		(do-one)))
  (define (do-movement)
    (while (d-StaffUp))
    (do-staff)
    (while (d-StaffDown)
        (do-staff)))
  (while (d-PreviousMovement))
  (do-movement)
  (while (d-NextMovement)
        (do-movement)))
(d-PopPosition)