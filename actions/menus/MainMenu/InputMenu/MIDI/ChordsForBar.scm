;;;;ChordsForBar
;;;allows you to play chords (over a bass) for the current bar - plays the bar first, goes on to next bar when you press space

(if (not (defined? 'ChordsForBar::active))
	(d-InfoDialog 
		(_ "This command expects you to play the notes of the current bar together with chords.
It will notate the chords in a new staff above the current staff. You can hold chords over several notes, change chords on one note.
To temporarily switch to listening so as to try out a chord depress the foot pedal.
It will play you two bars of the music at the start and after you have entered the chords.
Use the pitch bend control to go on to the next bar or to try again in the current bar.")))

(define-once ChordsForBar::active #f)

(if ChordsForBar::active
    (begin ;;;;Filter is already running stop it
      (disp "Turning off\n")
      (set! ChordsForBar::active #f)
      (d-PutMidi 0)
      (d-SetBackground #xFFFFFF)
      )
    (begin ;;;;Filter is not already running so run it
      ;;set up the pitch bend commands to re-run this ChordsForBar script (with the pitchbend command disabled because they flood)
      (set! Pitchbend::commandDown "(set! Pitchbend::commandDown \"(disp \\\"pitchbend down is disabled\n\\\")\") (d-ChordsForBar)")
      (set! Pitchbend::commandUp "(set! Pitchbend::commandUp \"(disp \\\"pitchbend up is disabled\n\\\")\") (d-MoveToMeasureRight)(d-ChordsForBar)")
     

      (let  ((ons '())(suspension #f)  (chord-position #f) (pedal #f))
	(define (noteOn? midi)
	  (= #x90 (bit-extract midi 0 8)))
	(define (noteOff? midi)
	  (= #x80 (bit-extract midi 0 8)))
	(define (pedalDown? midi)
	  (and (= #xB0 (bit-extract midi 0 8))  (= (bit-extract midi 8 16) #x40) (=  (bit-extract midi 16 24) #x7F)))
	
	(define (pedalUp? midi)
	  (and (= #xB0 (bit-extract midi 0 8))  (= (bit-extract midi 8 16) #x40) (=  (bit-extract midi 16 24) #x0)))
	(define (pause) (d-GetUserInput "Pausing..." "Press any key" " "))

	(define* (PlayCurrentMeasure #:optional (start 0))
	  (define ontime #f)
	  (define offtime #f)
	  (d-CreateTimebase)
	  (if (> start 0)
	    (begin
	      (d-PushPosition)
	      (let loop ((count start))
		(if (> count 0)
		  (begin
		    (d-MoveToMeasureLeft)
		    (loop (- count 1)))))
	      (set! ontime (CurrentMeasureOnTime))
	      (d-PopPosition))
	    (begin
	      (set! ontime (CurrentMeasureOnTime))))
	  (set! offtime (CurrentMeasureOffTime))
	  (d-SetPlaybackInterval ontime offtime)
	  (d-RestartPlay))
	  
	(define (amalgamate-ties)
	   ;;;start on a tied note. Amalgamate the note(s) tied to with the main note
	  (define this-dur  (string->number(car (string-split (d-GetNoteDuration) #\.))))
	  (define this-dots (d-GetDots))
	  (define continuing #t)
	  
	  (if (d-NextChordInMeasure)
	      (let ((next-dur (string->number (car (string-split (d-GetNoteDuration) #\.))))
		    (next-dots (d-GetDots)))
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
	      (if (not (d-NextChordInMeasure))
		  (set! continuing #f)))
	  (if (and continuing (d-IsTied))
	      (amalgamate-ties)))
	
;;starts on a rest, amalgamates with previous object if it is another rest
	(define (amalgamate-rests)
	  (define this-dur  (string->number(car (string-split (d-GetNoteDuration) #\.))))
	  (define this-dots (d-GetDots))
	  
	  (if (d-PrevChordInMeasure)
	      (if (Rest?)
		  (let ((prev-dur (string->number(car (string-split (d-GetNoteDuration) #\.))))
		    (prev-dots (d-GetDots)))
		    (cond
		     ((and (= this-dur prev-dur) (= this-dots prev-dots))		       
		      (d-Augment)
		      (d-NextChordInMeasure)
		      (d-DeleteObject))
		      ((and (= (/  this-dur 2) prev-dur) (= 0 this-dots) (= 0 prev-dots))
		       (d-Augment)
		       (d-AddDot)
		       (d-NextChordInMeasure)
		       (d-DeleteObject))
		      (else
		       (disp "Case not handled -did not join rests")
		       (d-NextChordInMeasure))))
		  (d-NextChordInMeasure))))
		      
;;;;seek back to the chord started but not finished (ie before the rests)
	(define (find-suspended-chord)
	  (let loop ()
	    (disp "looping")
	    (if (and (d-PrevChord) (Rest?))
		(loop))))
	
;;;start with cursor on the rest to be turned into a chord, end back at the same rest or rather the chord that has replaced it
	(define (continue-chord)
	  (define position #f)
	  (find-suspended-chord)
	  (set! position (GetPosition))
	  (let loop ()
	    (define denemodur #f)
	    (define numdots 0)
	    (d-SetMark)
	    (d-Copy)
	    (d-ToggleTie)
	    (d-NextChordInMeasure)
	    (set! denemodur (number->string (duration::lilypond->denemo (string->number (car (string-split (d-GetNoteDuration)  #\.))))))
	    (set! numdots (d-GetDots))
	    (d-Paste)
	    (d-DeleteObject)
	    (if (not (Appending?))
		(d-PrevChord))
	    (eval-string (string-append "(d-Change" denemodur ")" (if (> numdots 0) "(d-AddDot)" "")))
	    (if (d-NextChordInMeasure)
		(begin
		  (d-PrevChordInMeasure)
		  (loop))))
	  (apply d-GoToPosition position)
	  (amalgamate-ties))
	
	(define (add-note note)
	  (set! suspension #t)
	  (eval-string (string-append "(d-InsertNoteInChord \"" (d-GetNoteForMidiKey note) "\")"))	
	  (PlayNote (number->string note) 400))
	
	(define (GetChords bass-key)
	  (define chord-created #f)
	  (define triggerPedal #f)
	  (set! chord-position (GetPosition))
	  (if suspension
	      (continue-chord)
	      (for-each add-note ons))
	  (let loop ()
	    (let* (
		   (midi (d-GetMidi))
		   (velocity (bit-extract midi 16 24))
		   (note (bit-extract midi 8 16))
		   (command (bit-extract midi 0 8)))  
					
	      (cond
	       ((noteOn? midi)
		(set! ons (cons note ons));; use (null? ons) to test if empty
		(set! chord-created #t)	   
		(add-note note)
		(loop))
	       
	       ((noteOff? midi) 
		;(disp "A note off with chord-created=" chord-created "and suspension=" suspension "\n")
		(if (= note bass-key)				
		    (begin
		      (cond ((null? ons)
			     (continue-chord))
			    (else
			     #t;(disp "the else case\n" ons " and rest =" (Rest?) "\n")
			     )));;;finished getting chords for this bass note
		    (begin ;;; a note-off which is not the bass note
		      (set! ons (delq note ons))
		      (if (null? ons)
			  (begin
			    (set! suspension #f)
			    (set! chord-created #f)
				            ;;; creating a new chord over the same note.
			    (if (and pedal chord-position)
				(d-ToggleTie))
			    (cond 
			     ((Rest?)
			      (d-Diminish)
			      (d-SetMark)
			      (d-Copy)
			     ;(d-DirectiveDelete-chord "ChordsForBar")
			      (d-Paste)
			      )
			     ((> (d-GetDots) 0)
			      (d-SplitChord 3)
			      (d-MoveCursorLeft)
			      (d-MoveCursorLeft)
			      (d-MoveCursorLeft)
			      (d-Augment)
			      (d-MoveCursorRight)
			      (d-DeleteObject)
			      (ChangeToRest);on a whole chord: we remove notes all down to the Rest
			      )
			     (else
			      (d-SplitChord 2)
			      (d-MoveCursorLeft)
			      (ChangeToRest)
			      ))
			    (loop))
			  (loop)))))
	       ((or (zero? midi) (and  (= command #xE0) (> velocity 32)))
		(d-SetBackground #xFFFFFF)
		(disp "Abandoning getting chord\n"))

		
	       ((pedalUp? midi)
		(set! triggerPedal #t)
		(if (d-IsTied)
		    (d-ToggleTie))

		(set! pedal #f)
		(loop))
	       ((pedalDown? midi)
		(set! pedal #t)
		(loop))	      
	       
	       (else (loop)))))
	  (if (d-IsTied)
	      (d-ToggleTie))
	  (set! chord-position (GetPosition))) ;;; end of GetChords
	
	(define (createChordStaff)
	  (d-AddBefore)
	  (d-InitialClef "Treble")
	  (d-StaffProperties "denemo_name=Chords"))
	

	(define (EmptyContent)
	  (while (d-PrevObjectInMeasure))
	  (while (not (None?))
	      (d-DeleteObject))
	  (d-MoveToStaffDown))
	  
	(define (midi-thru)
	  (d-SetMidiThru #t)
	  (d-SetBackground #xB020B0)
	  (let loop ((midi (d-GetMidi)))
	      (if (not (pedalUp? midi))
		  (begin
		    (d-OutputMidi midi)
		    (loop (d-GetMidi)))
		  (d-SetMidiThru #f)))
	  (d-SetBackground #xB0E0B0))


	
;;;;;;;; actual code
	(set! ChordsForBar::active #t)
	
	(if  (equal? "Chords" (d-StaffProperties "query=denemo_name"))
	     (set! ChordsForBar::active (d-MoveToStaffDown)))
	
	(if (and ChordsForBar::active (d-MoveToStaffUp) (equal? "Chords" (d-StaffProperties "query=denemo_name")))
	    (EmptyContent)
	    (begin
	      (if  (equal? "Chords" (d-StaffProperties "query=denemo_name"))
		   (EmptyContent)		     
		   (begin
		     (d-MoveToStaffDown)
		     (createChordStaff)
		     (d-MoveToStaffDown)))))
		     
	(if ChordsForBar::active
	    (let ((bass-position (GetPosition)))  	      
	      (d-SetBackground #xB0E0B0)
	      (PlayCurrentMeasure 1)
	      (if (not (None?))
		  (let loop  ((bass-key (d-GetNoteAsMidi)))		    		    
		    
		    (d-PushPosition)
		    (d-SetMark)
		    (d-Copy) 
		    (d-DirectivePut-chord-graphic "ChordsForBar" "CheckMark")
		     (d-DirectivePut-chord-gy "ChordsForBar" -120)
		    (if (zero? bass-key)
			(begin
;;;; if it is a rest we are copying we need to remove the tick, and set chord-position #f since we cannot tie over it
;;;; we could also flag the situation to allow the pedal to put the chord on the whole beat including this rest.
			  (if (Rest?)
			      (begin
				(d-DirectiveDelete-chord "ChordsForBar")	
				(set! chord-position #f)
				))
			  (d-MoveToStaffUp)
			  (GoToMeasureEnd)
			  (d-Paste))
			  ;;;; listening waits for the bass-note to be played, meantime collecting up other notes in a list   
			(let listening ()
			  (let* ((midi (d-GetMidi))
				 (velocity (bit-extract midi 16 24))
				 (note (bit-extract midi 8 16))
				 (command (bit-extract midi 0 8)))  			    
			    (apply d-GoToPosition bass-position)
			    (cond ((and (= command #x90) (= note bass-key))
				    (if (and pedal chord-position)
				       (begin
					 (apply d-GoToPosition chord-position)
					 (d-ToggleTie)
					 (apply d-GoToPosition bass-position)))
				    (d-DirectiveDelete-chord "ChordsForBar")		       
				   
				    (PlayNote (number->string note) 30 "127")
				   
				    (d-MoveToStaffUp)
				    (GoToMeasureEnd)
				    (d-Paste)
				    (d-MoveCursorLeft)
				    (if (d-IsTied)
				       (d-ToggleTie))
				    (d-StagedDelete)

				    (if (and pedal (not chord-position))
				       (amalgamate-rests))
				    (GetChords bass-key))
				  ;;; a note on that is not the bass-note, put it in the list and go back to listening for bass note
				  ((= command #x90)
				   (d-PlayMidiKey midi)
				   (set! ons (cons note ons))
				   (listening))
				   
				  ;;; our synthetic 0 midi message - abort
				 ((zero? midi)				  
				 ;;  (d-PutMidi 0) ;;this clears the queue of intercepted events
				   (d-SetBackground #xFFFFFF)
				   (d-DirectiveDelete-chord "ChordsForBar")
				   (set! ChordsForBar::active #f))
				  
				  ((= command #x80)
				   (set! ons (delq note ons))
				   (if (null? ons)
				       (set! suspension #f))
				   (listening))
				   
				  ((pedalUp? midi)
				   (disp "Warning pedal up ignored\n")
				   (listening))
				   
 				  ((pedalDown? midi)
				   (midi-thru)
 				   (listening))
				  (else
				   ;;!!!!!! create a quiet drum sound hmm what about using the pitchbend to play the two bars... (d-PlayMidiKey #x5000)
				   (d-PlayMidiKey #x702709)
				   (disp "Ignoring " command " " note " " velocity " waiting for " bass-key "\n")
				   (listening))))))
		    (disp "finished with an object in the measure chords for bar is active is " ChordsForBar::active "\n")
		    (if ChordsForBar::active
			(begin 
			  (d-PopPosition)
			  
			  (if (d-NextObjectInMeasure)
			      (begin				
				(set! bass-position (GetPosition))		  
				(loop (d-GetNoteAsMidi)))
			      (begin
				(d-PutMidi 0)
				(PlayCurrentMeasure 1))))
			(begin
			    (d-PopPosition)
			    (d-MoveToMeasureRight))))
		  (disp "Finished ChordsForBar"))))
	(set! ChordsForBar::active #f)
	(d-SetBackground #xFFFFFF)
	;;(set! Pitchbend::commandUp "(d-CursorRight)")
	;;(set! Pitchbend::commandDown "(d-CursorLeft)")
	)))
;;;;;;;;End of ChordsForBar
