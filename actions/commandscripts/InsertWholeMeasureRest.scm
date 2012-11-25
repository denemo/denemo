;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;InsertWholeMeasureRest
 (if (or (None?)  (zero? (d-GetDurationInTicks)))
 	(begin
 		(d-WholeMeasureRest))
 	(begin	
 		(if (d-MoveToMeasureRight)
 			(d-WholeMeasureRest)
 			(begin
          			 (d-AppendMeasureAllStaffs)	
          			 (d-MoveCursorRight)
	  			  (d-WholeMeasureRest)))))
(d-MoveCursorRight)
(d-Set2)