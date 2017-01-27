;;;NonPrintingStaffs
(let ((tag "NonPrintingStaff") (ptr #f)(allmovements #f) (numstaffs (d-GetStaffsInMovement)) (thelist '()))
    (d-PushPosition)
    (d-MoveToMovementBeginning)
    (let loop ()
        (set! thelist (cons (cons (d-StaffProperties "query=denemo_name") (not (d-DirectiveGetForTag-staff tag))) thelist))
        (if (d-MoveToStaffDown)
            (loop)))
 
 (set! thelist (reverse thelist)) 
 (set! thelist (cons (cons (_ "Apply to All Movements") #t) thelist))
 
 (set! thelist (d-CheckBoxes thelist (_ "Choose Staffs to Print")))
 (set! allmovements (cdar thelist))
 (set! thelist (cdr thelist))

 (if thelist
    (begin
    	(if allmovements
    		(while (d-PreviousMovement)))
    	(let movement-loop ()
    		 (set! ptr thelist)
		(d-MoveToMovementBeginning) 
		(let loop ()
		    (if  (cdar ptr)
		        (d-NonPrintingStaff 'unset)
		        (d-NonPrintingStaff 'set))
		    (set! ptr (cdr ptr)) 
		    (if (d-MoveToStaffDown)
		        (loop)
		        (if (and allmovements (d-NextMovement))
		        	(movement-loop))))))
    (d-WarningDialog (_ "Cancelled")))
    (d-PopPosition))
  