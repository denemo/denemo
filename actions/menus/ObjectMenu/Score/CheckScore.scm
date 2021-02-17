;;;CheckScore
(define CheckScore::return #f)
(define-once CheckScore::ignore 0)
(define-once CheckScore::error-position #f)

(let ((old-volume (d-MasterVolume)))
    (d-MasterVolume 0)    
    (if CheckScore::params
        (set! CheckScore::ignore CheckScore::params))    
    (if (d-Directive-score? "CriticalCommentsAmended")
        (d-CriticalCommentary))
        
(if (not CheckScore::params);; interactive
	;;;Check Parts are same in each movement.
	(let ((parts #f))
		(define (collect-parts)
			(define parts (list (d-StaffProperties "query=denemo_name")))
			(while (d-MoveToStaffDown)
				(set! parts (cons (d-StaffProperties "query=denemo_name") parts)))
			parts)
		(define (differ list1 list2)
				(member #f (map equal? list1 list2)))
		(d-PushPosition)
		(d-GoToPosition 1 1 1 1)
		(set! parts (collect-parts))
		(while (d-NextMovement)
			(if (differ parts (collect-parts))
				(d-WarningDialog (string-append (_ "Parts in movement ")
					(number->string (d-GetMovement)) (_ " differ from those in opening movement.\nWhile this is not an error, be aware that trying to print a part that does not appear in each movement will result in empty movements.")))))
		(d-PopPosition)))        
        

    (while (d-PreviousMovement))
    (d-InfoDialog "Checking Score - Please wait\nThe display will be strange while checking is done!")
    (let movement ()
      (d-EvenOutStaffLengths)
      (while (d-MoveToStaffUp))   
      (let staff ()
        (d-KeepAlive) 
        (if (not (d-Directive-voice? "SubstituteMusic"))
            (begin
                (d-FixSlursInStaff)     
                (d-CheckTiesInStaff 'noninteractive)
                (if CheckTiesInStaff::return
                        (set! CheckScore::return CheckTiesInStaff::return))
                (if (not CheckScore::return)
                    (begin
                        (d-CheckDirectivePairs 'noninteractive)
                        (if CheckDirectivePairs::return
                            (set! CheckScore::return CheckDirectivePairs::return))))))
        (if (not CheckScore::return)
            (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
                (staff))))
      (if (not CheckScore::return)
          (begin
            (while (d-MoveToStaffUp))   
            (let staff ()
              (if (not (d-Directive-voice? "SubstituteMusic"))
            	(begin
				(d-MoveToBeginning)
				(let measure ()
				    (d-CheckTupletsInMeasure 'noninteractive)           
				    (set! CheckScore::return CheckTupletsInMeasure::return)
				    (if (not CheckScore::return)
				        (begin
				            (d-CheckBeamsInMeasure 'noninteractive)
				            (set! CheckScore::return CheckBeamsInMeasure::return)))
				    (if (not CheckScore::return)
				        (if (d-MoveToMeasureRight)
				          (measure))))))
		(if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
			(staff)))))
                               
         (if (not CheckScore::return)                      
                   (begin
                        (d-CheckTimeSignatures #t)
                        (set! CheckScore::return CheckTimeSignatures::return)))            
                               
        (if (and (not CheckScore::return) (not (d-Directive-layout? "PolymetricStaffs")))
              (begin
                (while (d-MoveToStaffUp))  
                  (let staff ()
                    (while (and (d-Directive-voice? "SubstituteMusic") (d-MoveToStaffDown)))
                    (if (not (d-Directive-voice? "SubstituteMusic"))
                        (begin
                            (d-MoveToBeginning)
                            (let measure ()
                                (if (not (LastMeasure?)) 
		                    (if (AcceptableDurationMeasure?)
		                                (begin
		                                    (d-MoveToMeasureRight)
		                                    (measure))
		                                (begin
		                                     (if (positive? CheckScore::ignore)
		                                            (begin
		                                                (set! CheckScore::ignore (1- CheckScore::ignore))
		                                                (d-MoveToMeasureRight)
		                                                (measure))
		                                            (begin
		                                            	(if (eq? (d-GetMeasure) 1)
		                                            	    (set! CheckScore::return (_ "Incorrect measure duration; If you are trying to create an upbeat (pickup/'anacrusis) use the Anacrusis command"))
		                                                    (set! CheckScore::return (_ "Incorrect measure duration")))
		                                                (set! CheckScore::error-position (GetPosition))))))))
                            (if (not CheckScore::return)
                                (begin
                                   
                                    (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
                                    (staff)))))))))
                        
        (if (not CheckScore::return)
            (begin
                (d-CheckBraces 'noninteractive)
                (set! CheckScore::return CheckBraces::Return)))
 
         (if (not CheckScore::return) 
         	(begin
         		(while (d-MoveToStaffUp))
         		(while (and (d-Directive-voice? "SubstituteMusic") (d-MoveToStaffDown)))
         		(d-MoveToEnd) 
         		(let ((ticks (GetMeasureTicks))) 
		      		 (let staff ()
					(d-KeepAlive) 
					(if (and (not (d-Directive-voice? "SubstituteMusic"))
				   			(not (eq? ticks (GetMeasureTicks))))
				   				(begin
				   					(set! CheckScore::error-position (GetPosition))
				   					(set! CheckScore::return (_ "Final Measures not all equal duration"))))
				   	(if (d-MoveToStaffDown)
				   		(staff))))))

        (if (not CheckScore::return)           
           (d-InstallGraceNoteHints))               
                                            
        (if (and (not CheckScore::return) (d-NextMovement))
                (movement)))
                
    (if (not CheckScore::params);; interactive
            (begin
                (if (not CheckScore::return)
                    (begin
                        (set! CheckScore::error-position #f)
                        (d-InfoDialog  (_ "No problem detected in this score")))
                    (begin
                        (if CheckScore::error-position
                            (apply d-GoToPosition CheckScore::error-position))
                        (d-InfoDialog CheckScore::return))))
            (disp "Error location " CheckScore::error-position "\n"))
    (d-MasterVolume old-volume))
