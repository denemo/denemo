;;;NonPrintingStaffs
(let ((tag "NonPrintingStaff") (ptr #f)(allmovements #f) (layout #f)(numstaffs (d-GetStaffsInMovement)) (thelist '()))
   (d-SelectDefaultLayout)   
    (d-PushPosition)
    (d-MoveToMovementBeginning)
    (let loop ()
        (set! thelist (cons (cons (d-StaffProperties "query=denemo_name") (not (d-DirectiveGetForTag-staff tag))) thelist))
        (if (d-MoveToStaffDown)
            (loop)))
 
 (set! thelist (reverse thelist)) 
  (set! thelist (cons (cons (_ "Do Not Create Layout for this selection") #t) thelist))
 (set! thelist (cons (cons (_ "Apply to All Movements") #t) thelist))
 
 (set! thelist (d-CheckBoxes thelist (_ "Choose Staffs to Print")))
 (set! allmovements (cdar thelist))
 (set! thelist (cdr thelist))
(set! layout (cdar thelist))
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
		        	(movement-loop)))))
    	(if (not layout)
    		(let ((name (d-GetUserInput (_ "Creating Layout") (_ "Give Layout Name") (_ "Woodwind"))))
    			(if (and name (positive? (string-length name)))
    				(begin
    					(d-CreateLayout name)
    					(d-SelectDefaultLayout))))))
        (d-WarningDialog (_ "Cancelled")))
    (d-PopPosition))
  