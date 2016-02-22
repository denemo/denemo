;;CheckTiesInStaff
(define-once CheckScore::ignore 0)
(define-once CheckScore::error-position #f)
(define CheckTiesInStaff::return #f)
(let ((position (GetPosition)))
    (d-MoveToBeginning)
    (let ((ok #t))  
      (let loop ()
        (if (and (d-IsTied) (Singlenote?))
          (let ((note (d-GetNote)))
                    (if (and (d-NextNote) (Singlenote?))
                        (let ((nextnote (d-GetNote)))
                            (if  (equal? note nextnote)
                                (loop)
                                (begin
                                    (if (positive? CheckScore::ignore)
                                            (set! CheckScore::ignore (1- CheckScore::ignore))
                                            (begin
                                    (set! position (GetPosition))                    
                                    (set! CheckTiesInStaff::return (_ "Tied notes not the same"))
                                    (set! ok #f))))))
                        (if (Singlenote?)
                            (begin
                                (if (positive? CheckScore::ignore)
                                    (begin
                                        (set! CheckScore::ignore (1- CheckScore::ignore))
                                        (loop))
                                    (begin  
                                        (set! position (GetPosition))                    
                                        (set! CheckTiesInStaff::return "No note to tie to")
                                        (set! ok #f))))))))
            (if (and ok (d-NextNote))
                (loop)))
  
        (if CheckTiesInStaff::return
            (set! CheckScore::error-position position))
        (if (not CheckTiesInStaff::params) ;;interactive
            (begin
                (if CheckTiesInStaff::return
                    (apply d-GoToPosition position)
                    (set! CheckTiesInStaff::return (_ "All ties in this staff are correctly placed")))
                (d-InfoDialog CheckTiesInStaff::return)))))
         
     
