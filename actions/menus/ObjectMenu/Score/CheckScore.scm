;;;;;;;;;CheckScore
(define CheckScore::return #f)
(define-once CheckScore::ignore 0)
(define-once CheckScore::error-position #f)
(if CheckScore::params
    (set! CheckScore::ignore CheckScore::params))
(if (d-Directive-score? "CriticalCommentsAmended")
    (d-CriticalCommentary))

(while (d-PreviousMovement))

(let movement ()
  (d-EvenOutStaffLengths)
  (d-InstallGraceNoteHints)
  (let staff ()
    (d-FixSlursInStaff)     
    (d-CheckTiesInStaff 'noninteractive)
    (if CheckTiesInStaff::return
            (set! CheckScore::return CheckTiesInStaff::return))
    (d-CheckDirectivePairs 'noninteractive)
    (if CheckDirectivePairs::return
            (set! CheckScore::return CheckDirectivePairs::return))
    (if (not CheckScore::return)
        (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
            (staff))))
  (if (not CheckScore::return)
      (begin
        (while (d-MoveToStaffUp))   
        (let staff ()
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
                      (measure)
                      (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
                           (staff))))))))
                           
     (if (not CheckScore::return)                      
               (begin
                    (d-CheckTimeSignatures #t)
                    (set! CheckScore::return CheckTimeSignatures::return)))            
                           
    (if (not CheckScore::return)
          (begin
            (while (d-MoveToStaffUp))  
              (let staff ()
                (d-ReBar 'noninteractive)
                (set! CheckScore::return ReBar::return)
                (if (not CheckScore::return)
                    (begin 
                        (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
                        (staff)))))))
                    
    (if (not CheckScore::return)
        (begin
            (d-CheckBraces 'noninteractive)
            (set! CheckScore::return CheckBraces::Return)))
                        
                                        
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
 
                   
