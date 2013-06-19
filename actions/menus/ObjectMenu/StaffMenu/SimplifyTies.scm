;;;;SimplifyTies
(let () 
(define (common-notes? this that)
  (define list1 (if this (string-split this #\space) '()))
  (define list2 (if that (string-split that #\space) '()))
  (let loop ()
    (if (or (null? list1) (null? list2))
	#f	
	(if (not (member (car list1) list2))
	    (begin
	      (set! list1 (list-tail list1 1))
	      (loop))
	    #t))))

  (define (amalgamate-ties)
    (define this-dur  (string->number(car (string-split (d-GetNoteDuration) #\.))))
    (define this-dots (d-GetDots))
    (define continuing #t)
    (define this-note (d-GetNotes))
    (if (d-NextChordInMeasure)
	(let ((next-note (d-GetNotes))(next-dur (string->number (car (string-split (d-GetNoteDuration) #\.))))
	      (next-dots (d-GetDots)))
	  (if (common-notes? this-note next-note)
	      (if (equal? this-note next-note)
		  (begin
		    (cond ((and (= this-dur next-dur) (= this-dots next-dots))
			   (d-DeletePreviousObject)
			   (d-Augment))
			  
			  ((and (= 1 this-dots) (= 0 next-dots) (= (/  next-dur 2) this-dur))
			   (d-DeletePreviousObject)
			   (d-RemoveDot)
			   (d-Augment)
			   (d-Augment))
			  
			  ((and (= (/  next-dur 2) this-dur) (= 0 this-dots) (= 0 next-dots))
			   (d-DeletePreviousObject)	       
			   (d-Augment)
			   (d-AddDot))
			  (else (disp "else case\n"))))
		  (begin
		    (d-PrevChord)
		    (d-ToggleTie)
		    (set! continuing #f)))
	       (begin ;;no notes in common for next in measure, delete tie and stop
		 (d-PrevChord)
		 (d-ToggleTie)
		 (set! continuing #f))))
	(if (d-NextChord)    ;;; no next chord in measure, try next measure		   
	    (let ((next-note (d-GetNotes)))
	      (if (not (common-notes? this-note next-note))
		  (begin
		    (d-PrevChord)
		    (d-ToggleTie)	     
		    (set! continuing #f))
		  (begin ;;; first chord in subsequent measure has some common notes
		    (set! continuing #f)
		    (d-PrevChord)
		    )))))
    (if (and continuing (d-IsTied))
	(amalgamate-ties)))

;;;;actual code
  (d-MoveToBeginning)
  (let loop ()
    (if (d-IsTied)
	(let ((this-note (d-GetNotes)))
	  (if (d-NextChord)
	      (if (common-notes? this-note (d-GetNotes))
		  (begin
		    (if (equal? this-note (d-GetNotes))
			(begin
			 (d-PrevChord)
			 (amalgamate-ties))))
		    (begin
		      (d-PrevChord)
		      (d-ToggleTie)
		      (d-NextChord)))
	      (begin ;;; no next chord so remove tie
	
		(d-ToggleTie)
		))))	      
    (if (d-NextChord)
	(loop))))
