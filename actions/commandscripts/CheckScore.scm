;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;;;;;;CheckScore
(define CheckScore::return #t)

(if (d-Directive-score? "CriticalCommentsAmended")
    (d-CriticalCommentary))

(while (d-PreviousMovement))
(let movement ()
  (d-EvenOutStaffLengths)
  (let staff ()
   	(d-FixSlursInStaff)   	
   	(d-CheckTiesInStaff)
   	(set! CheckScore::return CheckTiesInStaff::return)
   	(if CheckTiesInStaff::return
 	 	(if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
		(staff))))
   (if CheckScore::return
      (begin
   	(while (d-MoveToStaffUp))	
   	(let staff ()
 		(d-MoveToBeginning)
 		(let measure ()
 			(d-CheckTupletsInMeasure)			
 			(set! CheckScore::return CheckTupletsInMeasure::return)
 			(if CheckScore::return
 				(if (d-MoveToMeasureRight)
 					(measure))
  				 (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
				(staff)))))
	(if CheckScore::return
  	  (let staff ()
   	    (d-ReBar #t)
   	    (set! CheckScore::return ReBar::return)
      	    (if CheckScore::return
      			(begin 
				(if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
				(staff))))
				(if (d-NextMovement)
	  				(movement)))))))
