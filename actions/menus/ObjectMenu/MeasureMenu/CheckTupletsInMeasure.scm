;;;CheckTupletsInMeasure
(define-once CheckScore::ignore 0)
(define CheckTupletsInMeasure::return #f)
(let ()
 (define (get-pos)
	(string-append "\nAt movement " (number->string (d-GetMovement)) ",  voice " (number->string (d-GetStaff)) ", measure " (number->string (d-GetMeasure)) "."))
  (define start '())
  (while (d-PrevObjectInMeasure))
  (let loop ()
 ; (disp "loop " start " \n")
    (if (TupletOpen?)
      (begin
	(if (not (null? start))
	  (disp "Nested Tuplets"))
      (set! start (cons (GetPosition) start))))
	
    (if (TupletClose?)
      (if (null? start)
	    (begin
	   	 (if (positive? CheckScore::ignore)
				(begin
					(set! CheckScore::ignore (1- CheckScore::ignore))
					(set! start '())
					(if (d-NextObjectInMeasure)
								(loop)))
	      			(set! CheckTupletsInMeasure::return (string-append (_ "End Tuplet with no start") (get-pos)))))
	    (begin
	      (set! start (cdr start))
	      (if (d-NextObjectInMeasure)
		(loop))))      
      (if (d-NextObjectInMeasure)
	(loop))))
  (if (not (null? start))
    (begin
     		(if (positive? CheckScore::ignore)
				(begin
					(set! CheckScore::ignore (1- CheckScore::ignore)))
     				(begin
     					 (apply d-GoToPosition (car start))
      					(set! CheckTupletsInMeasure::return (string-append (_ "Start Tuplet with no end")  (get-pos)))))))
   (if (not CheckTupletsInMeasure::params)
		(begin ;; interactive
			(if (not CheckTupletsInMeasure::return)
				(begin
					(set! CheckTupletsInMeasure::return (_ "All tuplets in measure are terminated"))))
			(d-InfoDialog CheckTupletsInMeasure::return))))   
      
    