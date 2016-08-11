;;;DeleteMeasuresBeforeCursor
(let ((choice DeleteMeasuresBeforeCursor::params))
	 (if (not choice)
	 	(set! choice	(RadioBoxMenu (cons (_ "Cancel") #f) (cons (_ "All Staffs - Delete all measures from cursor?") 'all) (cons (_ "This Staff/Voice only?") 'this))))
                 (case choice
                    ((all)
                    	(d-SetSaved #f)
                    	(d-EvenOutStaffLengths)
                        (while (d-MoveToMeasureLeft)
                            (d-DeleteMeasureAllStaffs)))
                   ((this)
                        (d-SetSaved #f)
                        (while (d-MoveToMeasureLeft)
                            (d-DeleteMeasure)))
                    (else        
                        (d-InfoDialog (_ "Cancelled")))))
