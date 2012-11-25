;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;; Warn about incomplete measures
(d-PushPosition)
    ;;;;This will be MoveToFirstIncompleteMeasure
(d-MoveToBeginning)
(while (d-MoveToStaffUp)
       ;;; is an expression needed here?
       #t)

(let loop ()
  (if (and  (MeasureComplete?) (d-MoveToMeasureRight) (MeasureComplete?))
      (loop)
      (if (and (MeasureComplete?) (d-MoveToStaffDown))
	  (begin (d-MoveToBeginning) 
		 (loop)))))
(if (MeasureComplete?)
    (begin
      (d-PopPosition)
      (d-WarningDialog "All measures appear complete - re-run Anacrusis etc if you have printing problems"))
    (begin 
      (d-WarningDialog "There seem to be incomplete measures; this may print strangely")
      (d-PopPushPosition)
      (d-PopPosition)))

