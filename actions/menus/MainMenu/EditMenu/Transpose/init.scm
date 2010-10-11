;;; Initialize Transpose routines.
(if (not (defined? 'Transpose::init))
    (define Transpose::init #f))
(if (not Transpose::init)
    (begin
;;;; public variables
      (define Transpose::SetTransposeInterval 0)
      (define Transpose::TransposeNote 0)
      (define Transpose::TransposeNoteList 0)
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
	      (set! Transpose::transpose-delta (Transpose::lilyname->pitch note)) 
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

      (define Transpose::pitch->lilyname
        (lambda (pitch)
  	  (let ((octave->text 0)(accidental->text 0)(pitch->text 0))
    	    (begin
              (set! octave->text
                (lambda (octave_num)
                  (let ((octave_string "")
                        (apply_octave 0))
                    (set! apply_octave
                      (lambda (string value)
                        (begin
                          (if (< value 0)
                            (begin
                              (set! octave_string (string-append octave_string ","))
                              (apply_octave string (+ value 1))))
                          (if (> value 0)
                            (begin
                              (set! octave_string (string-append octave_string "'"))
                              (apply_octave string (- value 1))))
                        )))
                        (apply_octave octave_string octave_num)
                 	octave_string
           	   )))

		(set! accidental->text
		  (lambda (accidental_num)
		  (let ((accidental_string "")
			(apply_accidental 0))
		    (set! apply_accidental
		      (lambda (string value)
			(begin
			  (if (> value 0)
			    (begin
			      (set! accidental_string (string-append accidental_string "is"))
			      (apply_accidental string (- value 1))))
			   (if (< value 0)
			    (begin
			      (set! accidental_string (string-append accidental_string "es"))
			      (apply_accidental string (+ value 1))))
			 )))
		         (apply_accidental accidental_string accidental_num)
			 accidental_string
		   )))

		(set! pitch->text
		  (lambda (pitch_num)
		   (let ((pitch_string ""))
		    (set! pitch_string (integer->char (+ (modulo (+ pitch_num 2) 7) 97)))
		    (string pitch_string)
		    )))
		
		(string-append
		  (pitch->text (cadr pitch))
		  (accidental->text (caddr pitch))
		  (octave->text (car pitch)))

	     )
	     )))

      (define Transpose::lilyname->pitch
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
      
      ;check to see if this is really needed
      (define Transpose::transposed 
        (lambda ()
	  (begin
	    (Transpose::transpose Transpose::original-pitch 
			      Transpose::original-delta ))))

      ;This is in use by the Edit->Transpose->Transpose Selection script.
      (define Transpose::TransposeNote 
        (lambda ()
	  (let ( (numofnotes 0) 
	  	 (notelist '())
		 (eachnote 0)
		 (process_notelist 0)
		 (transposed_notelist ""))

	    (begin
	      (set! process_notelist
		(lambda (note)
		  (set! Transpose::original-pitch (Transpose::lilyname->pitch note))
		        (set! transposed_notelist 
		        (string-append transposed_notelist (Transpose::pitch->lilyname(Transpose::transposed))))
			(set! transposed_notelist (string-append transposed_notelist " "))
		      ))
	      (set! notelist (d-GetNotes))
	      (if (string? notelist)
		  (set! notelist (string-split (d-GetNotes) #\space))
		  (set! notelist '()))
	      (set! numofnotes (length notelist))
	      ;(display "numofnotes = ")
	      ;(display numofnotes)
	      ;(newline)
	      (if (= numofnotes 1)
	        (begin
	    	  (set! Transpose::original-pitch (Transpose::lilyname->pitch (d-GetNotes)))
	          (d-ChangeChordNotes (Transpose::pitch->lilyname(Transpose::transposed)))))	  
	      (if (> numofnotes 1)
	        (begin
		  (map process_notelist notelist)
		  (d-ChangeChordNotes transposed_notelist)
		  )) 
	    ))))
     
      (define Transpose::TransposeNoteList 
        (lambda (string_of_notes)
	  (let ( (numofnotes 0) 
	  	 (outputlist '())
		 (eachnote 0)
		 (process_notelist 0)
		 (transposed_notelist ""))

	    (begin
	      (set! process_notelist
		(lambda (note)
		  (set! Transpose::original-pitch (Transpose::lilyname->pitch note))
		        (set! transposed_notelist 
		        (string-append transposed_notelist (Transpose::pitch->lilyname(Transpose::transposed))))
			(set! transposed_notelist (string-append transposed_notelist " "))
		      ))

	      (set! outputlist (string-split string_of_notes #\space))
	      (set! numofnotes (length outputlist))
	      (display "numofnotes = ")
	      (display numofnotes)
	      (newline)
	      (if (= numofnotes 1)
	        (begin
	    	  (set! Transpose::original-pitch (Transpose::lilyname->pitch string_of_notes))
	          (set! transposed_notelist (string-append (Transpose::pitch->lilyname(Transpose::transposed))))
		  ))	  
	      (if (> numofnotes 1)
		  (map process_notelist outputlist))
		  transposed_notelist
	    ))))

      (define Transpose::init #t)))




