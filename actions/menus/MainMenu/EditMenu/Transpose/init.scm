;;; Initialize Transpose routines.
(if (not (defined? 'Transpose::init))
    (define Transpose::init #f))
(if (not Transpose::init)
    (begin
;;;; public variables
      (define Transpose::SetTransposeInterval 0)
      (define Transpose::TransposeNote 0)
      (define Transpose::Note "b,")
      (define Transpose::Interval "c b,")

;;;;;;;;;;; private variables
					;original note
      (define Transpose::original-pitch '(0 0 0))
					;transposition amount
      (define Transpose::original-delta '(0 0 0))

      (define Transpose::transpose-origin '(0 0 0))
      (define Transpose::transpose-delta '(0 0 0))
;;;;;;;;;; code

      (define (Transpose::SetTransposeInterval note)
	    (begin
	      (set! Transpose::transpose-delta (Transpose::lily->pitch note)) 
	      (Transpose::get-delta)))

      (define (Transpose::get-interval-from-selection)
	(let ((first-note "")
	      (second-note ""))
	  (set! first-note (d-GetNote))
	  (NextChordInSelection)
	  (set! second-note (d-GetNote))
	  (string-append first-note " " second-note)
	  ))

      (define (Transpose::GetTransposeInterval)
	(d-GetUserInput "Setting a Transposition Interval" 
	   "Enter a note, a space, and the note you wish this to transpose to" "c d"))

      (define Transpose::lily->pitch
	(lambda (lilyname)
	  (let ((accidental 0) (octave 0) (notename 0) (loop 0))
	    (begin
	      (set! notename 
		    (lambda (char)
		      (modulo (- (char->integer char) 99) 7)
		      ))
	      (set! loop 
		    (lambda (x)
		      (if (< x (string-length lilyname))
			  (begin
			    (if (= x 0) (set! notename (notename (string-ref lilyname x))))
			    (if (> x 0) 
				(begin
				  (if (equal? #\i (string-ref lilyname x))
				      (set! accidental (+ accidental 1)))
				  (if (equal? #\e (string-ref lilyname x))
				      (set! accidental (- accidental 1)))
				  (if (equal? #\' (string-ref lilyname x))
				      (set! octave (+ octave 1)))
				  (if (equal? #\, (string-ref lilyname x))
				      (set! octave (- octave 1)))))
			    (loop (+ 1 x))
			    )
			  );end of if
		      )
		    );end of loop
	      (loop 0)
	      )
	    `(,octave ,notename ,accidental)

	    );end of let
	  ))
	;;;;copied from chord-name.scm in lilypond-1.6.5

      (define Transpose::semitone-vec (list->vector '(0 2 4 5 7 9 11)))

      (define Transpose::semitone 
	(lambda (pitch)
	  (+ (* (car pitch) 12)
	     (vector-ref Transpose::semitone-vec (modulo (cadr pitch) 7))
	     (caddr pitch))))

      (define Transpose::transpose 
	(lambda (pitch delta)
	  (let ((simple-octave (+ (car pitch) (car delta)))
		(simple-notename (+ (cadr pitch) (cadr delta))))
	    (let ((octave (+ simple-octave (quotient simple-notename 7)))
		  (notename (modulo simple-notename 7)))
	      (let ((accidental (- (+ (Transpose::semitone pitch) (Transpose::semitone delta))
				   (Transpose::semitone `(,octave ,notename 0)))))
		`(,octave ,notename ,accidental))))))

      (define Transpose::get-delta
	(lambda ()
	  (begin
	    (set! Transpose::original-delta (Transpose::transpose Transpose::transpose-origin Transpose::transpose-delta)))))

      (define Transpose::split-input
	(lambda (arguments)
	  (let ( 
		(first (list-ref (string-split arguments #\space) 0))
		(second (list-ref (string-split arguments #\space) 1))
		)
	    (set! Transpose::transpose-origin (Transpose::lily->pitch first))
	    (set! Transpose::transpose-delta (Transpose::lily->pitch second))
	    )))

      (define Transpose::transposed (lambda ()
				      (begin
					(Transpose::transpose Transpose::original-pitch 
							      Transpose::original-delta ))))

      (define Transpose::transposed-diff (lambda ()
					   (begin
					     (let
						 ((octave (- (list-ref (Transpose::transposed) 0) (list-ref Transpose::original-pitch 0)))
						  (notename (- (list-ref (Transpose::transposed) 1) (list-ref Transpose::original-pitch 1)))
						  (accidental (list-ref (Transpose::transposed) 2))
						  )
					       `(,octave ,notename ,accidental)))))

      (define Transpose::TransposeNote (lambda ()
				       (begin 
					 (set! Transpose::original-pitch (Transpose::lily->pitch (d-GetNotes)))
					 (d-DiatonicShift (number->string (* 7 (list-ref (Transpose::transposed-diff) 0))))
					 (d-DiatonicShift (number->string (list-ref (Transpose::transposed-diff) 1)))
					 (if (= (list-ref (Transpose::transposed-diff) 2) 2) 
					     (begin
					       (d-Sharpen) 
					       (d-Sharpen)
					       ))
					 (if (= (list-ref (Transpose::transposed-diff) 2) 1) (d-Sharpen))
					 (if (= (list-ref (Transpose::transposed-diff) 2) -2) 
					     (begin 
					       (d-Flatten)
					       (d-Flatten)
					       ))
					 (if (= (list-ref (Transpose::transposed-diff) 2) -1) (d-Flatten)))))
      (define Transpose::init #t)))




