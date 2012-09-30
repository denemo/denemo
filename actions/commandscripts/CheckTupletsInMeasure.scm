;;;CheckTupletsInMeasure
(define CheckTupletsInMeasure::return #t)
(let ()
 (define (get-pos)
	(string-append "\nAt movement " (number->string (d-GetMovement)) ",  voice " (number->string (d-GetStaff)) ", measure " (number->string (d-GetMeasure)) "."))
  (define start '())
  (let loop ()
  (disp "loop\n")
    (if (TupletOpen?)
      (begin
	(if (not (null? start))
	  (disp "Nested Tuplets"))
      (set! start (cons (GetPosition) start))))
	
    (if (TupletClose?)
      (if (null? start)
	    (begin
	      (set! CheckTupletsInMeasure::return #f)
	      (d-InfoDialog (string-append (_ "End Tuplet with no start") (get-pos)))    
	      )
	    (begin
	      (set! start (cdr start))
	      (if (d-NextObjectInMeasure)
		(loop))))
	      
      (if (d-NextObjectInMeasure)
	(loop))))
  (if (not (null? start))
    (begin
      (apply d-GoToPosition (car start))
      (set! CheckTupletsInMeasure::return #f)
      (d-InfoDialog (string-append (_ "Start Tuplet with no end")  (get-pos))))))
    