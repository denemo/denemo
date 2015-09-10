;;; CheckForIncompleteMeasures
(d-PushPosition)
(d-MoveToBeginning)
(while (d-MoveToStaffUp))
(let loop ()
  (if (and  (MeasureComplete?) (d-MoveToMeasureRight) (MeasureComplete?))
      (loop)
      (if (and (MeasureComplete?) (d-MoveToStaffDown))
	  (begin (d-MoveToBeginning) 
		 (loop)))))
(if (MeasureComplete?)
    (begin
      (d-PopPosition)
      (d-WarningDialog (_ "All measures appear complete.")))
    (begin 
      (d-WarningDialog (_ "This measure has the wrong duration."))
      (d-PopPushPosition)
      (d-PopPosition)))

