;;;;;;;;;CheckScore
(define CheckScore::return #f)
(define-once CheckScore::ignore 0)
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
    (set! CheckScore::return CheckTiesInStaff::return)
    (if (not CheckTiesInStaff::return)
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
          (let staff ()
            (d-ReBar #t)
            (set! CheckScore::return ReBar::return)
            (if (not CheckScore::return)
                (begin 
                    (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
                    (staff))))))
                    
    (if (not CheckScore::return)
        (begin
            (d-CheckBraces 'noninteractive)
            (set! CheckScore::return CheckBraces::Return)))
                        
                                        
    (if (and (not CheckScore::return) (d-NextMovement))
            (movement)))
            
(if (not CheckScore::params);; interactive
        (begin
            (if (not CheckScore::return)
                    (set! CheckScore::return (_ "No problem detected in this score")))
            (d-InfoDialog CheckScore::return)))
 
                   
