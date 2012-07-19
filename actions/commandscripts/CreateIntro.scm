;;;;;;;;;;CreateIntro
(let ( (position (GetPosition)) (timesig (d-InitialTimeSig "query=timesigname")) (numerator #f)(denominator #f))
  (define (createIntroStaff)
    (d-AddBefore)
    (d-InitialClef "Treble")
    (d-StaffProperties "denemo_name=Intro"))
  
  (define (writeIntroBar numerator denominator)
;;;write an intro bar
    (let loop ((count (string->number numerator)))
      (if (positive? count)
	  (begin
	    (eval-string (string-append "(d-" (number->string (duration::lilypond->denemo (string->number denominator))) ")"))
	    (loop (- count 1))))))
  
  
  (define (deleteToEnd)
    (d-SetMark)
    (d-GoToEnd)
    (d-Cut))
  
  (define firstmeasure #t)
  (define measurenum (list-ref position 2))
  (if (d-MoveToStaffUp)
      (begin
	(if (equal? "Intro" (d-StaffProperties "query=denemo_name"))
	    (begin
	      (set! firstmeasure #f);;we will not need to add an initial intro measure, as there will be one already
	      (if (not (None?))
		  (if  (equal? "y" (d-GetUserInput"Non Empty Intro staff" "Remove the previous transcription from this measure on?" "y"))
		       (deleteToEnd)
		       (set! firstmeasure 'abort))))
	    (begin
	      (d-MoveToStaffDown)
	      (createIntroStaff))))
      (begin	
	(createIntroStaff)))
  (if (eq? firstmeasure 'abort)
      #f
      (begin	
	(if firstmeasure
	    (begin	
	      (d-GoToBeginning)
	      (set! measurenum (+ 1 measurenum))
	      (d-InsertMeasure)
	      (set! numerator (car (string-split   timesig #\/)))
	      (set! denominator (cadr (string-split  timesig #\/)))
	      (writeIntroBar numerator denominator)))		
	(d-MoveToStaffDown)
	(d-GoToPosition #f #f  measurenum (list-ref position 3)))))
      