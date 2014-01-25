;;CheckTiesInStaff
(define-once CheckScore::ignore 0)
(define CheckTiesInStaff::return #f)
(let ((position (GetPosition)))
(d-MoveToBeginning)
(let ((ok #t))	
  (let loop ()
    (if (and (d-IsTied) (Singlenote?))
      (let ((note (d-GetNote)))
				(if (and (d-NextChord) (Singlenote?))
					(let ((nextnote (d-GetNote)))
						(if (or (equal? note nextnote) (positive? CheckScore::ignore))
							(begin
								(if (positive? CheckScore::ignore)
										(set! CheckScore::ignore (1- CheckScore::ignore)))
							   (if (d-NextChord)
								(loop)))
							(begin
								(set! CheckTiesInStaff::return (_ "Tied notes not the same"))
								(set! ok #f))))
					(if (Singlenote?)
						(begin
							(if (positive? CheckScore::ignore)
								(begin
									(set! CheckScore::ignore (1- CheckScore::ignore))
									(loop))
								(begin						
									(set! CheckTiesInStaff::return "No note to tie to")
									(set! ok #f))))))))
		(if (and ok (d-NextChord))
			(loop)))
	(if CheckTiesInStaff::return
		(set! CheckScore::position position))
	(if (not CheckTiesInStaff::params)
		(begin ;; interactive
			(if (not CheckTiesInStaff::return)
				(begin
					(apply d-GoToPosition position)
					(set! CheckTiesInStaff::return (_ "All ties in this staff are correctly placed"))))
			(d-InfoDialog CheckTiesInStaff::return))
 		(if (not CheckTiesInStaff::return)
     			(apply d-GoToPosition position)))))
     
     
