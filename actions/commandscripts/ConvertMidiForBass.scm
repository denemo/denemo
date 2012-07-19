;;;;;;;;;DenemoConvertForBass
(let ()
(d-PushPosition)
(defstruct note name start duration)
(define Notes '())

(define (find-note key)
					;searches forward from index through Notes looking for a note with pitch key returns the index where this is found or #f
  (let loop ((current-index 0))
    (if (< current-index (length Notes))
	    (let ((current (car (list-ref Notes current-index))))
	      (if (not (eq? (note.name current) key))
		  (loop (+ current-index 1))
		  current-index))
	#f)))
	
(if (d-RewindRecordedMidi)
    (let looprecordednotes ((note #f)(tick 0))
      (set! note (d-GetRecordedMidiNote))
      (if note
	  (begin
	    (set! tick (d-GetRecordedMidiOnTick))
	    (if (< tick 0)
		(let ((note-index (find-note note)))
		  (if note-index
		      (let ((thenote (car (list-ref Notes note-index))))							
			(set!note.duration thenote  (- (- tick) (note.start thenote)))			
			(looprecordednotes note tick))			
		       (let ((firstnotestart (note.start (car (list-ref Notes 0)))))	
		       ;;; no note-on for this note-off, put one at the start
		       	(disp "no on for " note " \nUsing " (- (- tick) firstnotestart) "\n")
		      (set! Notes (cons (list (make-note 'name note 'start firstnotestart 'duration (- (- tick) firstnotestart)  )) Notes))		       
		      	(looprecordednotes note tick))
		      ))
		(begin   ;;; a note onset, put it in the list Notes.
		  (set! Notes (cons (list (make-note 'name note 'start tick 'duration 384)) Notes))
		  (looprecordednotes note tick))))
	  (begin         ;;;;;; finished generating Notes as a list of lists each with one note struct in it
	;  (disp Notes)
	    (if (> (length Notes) 0)
		(let ()
;;;;;;; we have at least one note in the Notes list, define to procs to create a denemo chord and add a note to it
		    (define (add-note note)
		      (if (note? note)
			  (begin
			  (eval-string (string-append "(d-InsertNoteInChord \"" (d-GetNoteForMidiKey (note.name note)) "\")")))
			  (format #t "\tNo note to add note ~a ~a to\n" (note.name note) (note.duration note))))
		    

(define (get-durationInsert ticks)
  (cond
	 ( (= ticks 6)  "(d-Insert8)")       ;1/256
	 ( (= ticks 9)  "(d-Insert8)(d-AddDot)")       ;1/256.

 	 ( (= ticks 12)  "(d-Insert7)")   ;1/128
	 ( (= ticks 18)  "(d-Insert7)(d-AddDot)")   ;1/128.

 	 ( (= ticks 24)  "(d-Insert6)")   ;1/64
	 ( (= ticks 36)  "(d-Insert6)(d-AddDot)")   ;1/64.

 	 ( (= ticks 48) "(d-Insert5)")   ;1/32
 	 ( (= ticks 72) "(d-Insert5)(d-AddDot)")   ;1/32.

 	 ( (= ticks 96)  "(d-Insert4)")   ;sixteen 1/16
 	 ( (= ticks 144)  "(d-Insert4)(d-AddDot)")   ;sixteen 1/16.

 	 ( (= ticks 192) "(d-Insert3)" ) ; eight 1/8
 	 ( (= ticks 288) "(d-Insert3)(d-AddDot)" ) ; eight 1/8.

 	 ( (= ticks 384)  "(d-Insert2)") ; quarter 1/4
 	 ( (= ticks 576)  "(d-Insert2)(d-AddDot)") ; quarter 1/4.

 	 ( (= ticks 768)   "(d-Insert1)") ; half 1/2
 	 ( (= ticks 1152)   "(d-Insert1)(d-AddDot)") ; half 1/2.

 	 ( (= ticks 1536)  "(d-Insert0)" ) ; whole 1
 	 ( (= ticks 2304)  "(d-Insert0)(d-AddDot)" ) ; whole 1.

 	 ( (= ticks 3072)  "(d-Breve)" ) ; breve 2*1
 	 ( (= ticks 4608)  "(d-Breve)(d-AddDot)" ) ; breve 2*1.

 	 ( (= ticks 6144)  "(d-Longa)") ; longa 4*1
 	 ( (= ticks 9216)  "(d-Longa)(d-AddDot)") ; longa 4*1.

 	 ( (= ticks 12288)  "(d-Maxima)")  ; maxima 8*1
 	 ( (= ticks 18432)  "(d-Maxima)(d-AddDot)")  ; maxima 8*1.

	(else "(d-Insert3)(d-ToggleGrace)"))) 






(define (insert-note name dur)
  (eval-string (string-append  (get-durationInsert dur)"\n"))
  (d-PutNoteName (d-GetNoteForMidiKey name)))
;;;;;;;;;;;;;; end of defining procs
		    
;;; notes has been set up by pre-pending so it is backwards...	    
		    (set! Notes (reverse Notes))
     (disp "The set of notes recorded was " Notes "\n")
                    ;;;;;; take the Notes and seek out bass notes, remove them and form chords, insert chords in staff above
		    (let () ;;;no loop here we drive it via the loopforbasskey


                      ;;;;;;;;;;; overlap decides if two notes should be a chord
		      ;;;;;;;;;;; the criterion is simply if they start close together
		      (define (overlap n1 n2)
			
			(< (abs (- (note.start n1) (note.start n2))) 50))
                      ;;;;;;;;;;;;;;;;;; end of overlap
		      
					;(format #t "Number of notes ~a\n" index)

                      ;;;;;;;Step through notes in current (bass) staff, placing chords in (empty) staff above
		    
				(let loopforbasskey ()

;;;;;;;;;;;;; copy non-notes to the empty staff above and stop with the cursor on the first bass note
				(let loop ()
                                  ;;;;first copy anything that are not a bass note to the staff above
				  (if (or (Rest?) (Tupletmarker?))
				      (begin 
					(d-SetMark) (d-Copy) (d-PushPosition)(d-MoveToStaffUp) (GoToMeasureEnd)(d-Paste)(d-PopPosition))
				      (begin
					(if (and (not (Note?)) (d-NextObject))
					    (loop)))))

;;;;;;;;;;;;; cursor is on the first/next bass note (if any)
				(if (Note?)  
				    (let  ((bass-key (d-GetNoteAsMidi)) (bass-duration (d-GetDurationInTicks))) ;;;midi number and duration of current note
				      ;; now loop through the Notes list looking for a note the same MIDI key number, which should be before ticks move on too much... once found, consecutive notes are taken to make a chord or chords for the bass note, an
				      (define notes-belonging '());the notes belonging to the bass note
				      (define bass-note #f);a bass note (list) selected from Notes

;;;;;;;;;;;;;;;;;; these procs: make-chords takes a list which has each note in a separate list and puts all the (consecutive) ones that overlap into a single list, that is it turns a sequence of list-of-note into a list-of-notes which represents a chord. It returns the list.
		      
				      (define (make-chords notes)
					(let loop ((index 0))
					  (let ((note1 (list-ref notes index))
						(note2 #f))
					    (if (> (length notes) (+ 1 index))
						(begin
						  (set! note2 (list-ref notes (+ 1 index)))
						  (if (overlap (car note1) (car note2))
						      (begin
							(list-set! notes index (cons (car note2) note1))
							(set! notes (delq note2 notes)) ;;so we loop with same index
							)
						      (set! index (+ 1 index)))
						  (loop index)))))

					notes)	
;;;;;;; insert-chord takes a list of note structs and puts a chord in the staff above with those notes.		
				      (define (insert-chord chord duration)
				      (disp "making a chord " duration "\n")
						(insert-note (note.name (car chord)) duration)
							(for-each  add-note (cdr chord)))

				      (define (make-tied)
				      (disp "tying ...\n")
					(d-PrevNote)
					(d-ToggleTie)
					(d-NextNote))


;;;;;;;;push back the chord as a set of notes with the given start and duration
				      (define (push-back chord thestart theduration)
					(let ((names '()))
					  (set! names (map note.name chord))
					  (let loop ((index 0))
					    (if (> (length names) index)
						(begin
						  ;(disp "Notes before " Notes "\n")
						  (set! Notes (cons (list (make-note 'name (list-ref names index)
										     'start thestart
										     'duration theduration)) Notes))
						  ;(disp "Notes after " Notes "\n")
						  (loop (+ index 1)))))))

;;;;;;;;;;; suggest duration returns a number of ticks that should be used when dur ticks are found in a bass note of bass-dur
				      ;; (define (suggest-duration dur bass-dur)
;; 					(let ((fraction (/ dur bass-dur)))
;; 					  (disp "suggesting for fraction " fraction "\n")
;; 					  (cond 
;; 					   ( (< (abs (- fraction 1)) 0.1) bass-dur)
;; 					   (  (< (abs (- fraction 0.5)) 0.1) (/ bass-dur 2))
;; 					   (  (< (abs (- fraction (/ 1 3))) 0.05) (/ bass-dur 3))
;; 					   (  (< (abs (- fraction 0.25)) 0.05) (/ bass-dur 4))
;; 					   (  (< (abs (- fraction 0.75)) 0.1) (/ bass-dur (/ 4 3)))
;; 					   (else bass-dur)
;; 					   )
;; 					))
				      

				      (define (suggest-duration dur bass-dur)
					(let ((fraction (/ dur bass-dur)) (thelist #f))
					  (disp "suggesting for fraction " fraction "\n")
					  (set! thelist (list 
							 (cons (abs (- fraction 1)) bass-dur)
							 (cons (abs (- fraction 0.5))   (/ bass-dur 2))
							; (cons(abs (- fraction (/ 1 3)))  (/ bass-dur 3))
							 (cons (abs (- fraction 0.25))   (/ bass-dur 4))
							 (cons (abs (- fraction 0.75))  (/ bass-dur (/ 4 3)))))
					  (let loop ()
					    (if (> (length thelist) 1)
						(let ((poss1 (list-ref thelist 0)) (poss2 (list-ref thelist 1)))
(disp "poss 1 " poss1 "and poss2 " poss2 "\n")
						  (if (< (car poss1) (car poss2))
						      (set! thelist (delq poss2 thelist))
						      (set! thelist (delq poss1 thelist)))
						  (loop))
						(cdr (list-ref thelist 0))))))
					
                                       ;;;;;;; insert-chords takes a list of lists of note structs and the duration of the denemo bass note they have been assigned to. It inserts a chord for each of the lists assigning durations to fit the bass note.If enough of the last chord duration remains a tie is issued on the chord and then the chord is pushed back onto the global Notes list with start and duration modified to suit, so that it will be suspended over the next bass note		
				      (define (insert-chords notes bass-duration)
                                        ;(disp "insert-chords called with " notes "\n")	
					
					(let ((chords (make-chords notes)) (remaining bass-duration))
				    ;;;loop through the chords, getting a good duration value, the duration from one to the next and inserting
					 ; (disp "We have chords " chords "\n")
					  (let loop ((index 0))
					    (if (> (length chords) (+ 1 index))
						(let ((chord1 (list-ref chords index))
						      (chord2 #f)
						      (duration #f))
						  
						  (set! chord2 (list-ref chords (+ 1 index)));;move this into the init
						  ;;;if chord1 hangs on into chord2 shorten it
						    (if (contains (car chord1) (car chord2))
			  (set!note.duration (car chord1) (- (note.start (car chord2)) (note.start (car chord1)))))
		      
						  
						  
						  
						  (set! duration (- (note.start (car chord2)) (note.start (car chord1))))
						  (set! duration (suggest-duration duration bass-duration))
						  (set! remaining (- remaining duration))
						   ;;  (if (< remaining 50)
;; 						      (begin
;; 							(set! duration (+ duration remaining))
;; 							(set! remaining 0)))
							      

						  ;(disp "Insert chord " chord1 " With duration " duration "\n")
						  (insert-chord chord1 duration)
						  (loop (+ index 1)))	     
						(begin
						  (if (= (length chords) (+ 1 index))
						      (let ((chord (list-ref chords index)) (duration #f))
							
							;(disp "the remaining duration is " remaining " for chord " chord" \n")
							(if (positive? remaining)
							    (begin 
							      (set! duration (suggest-duration remaining bass-duration))
							      (set! remaining (- remaining duration))
							      (insert-chord chord duration)
							      (disp "if remaining is still too much push back " remaining "\n")
							      (if (positive? remaining)
								  (let ((n2 (car chord)))
								    (if (> (/ (- (note.duration n2) remaining)
									      (note.duration n2)) 0.2)
									(begin
									  (make-tied)
									  (push-back chord (+ (note.start n2)  remaining) (- (note.duration n2) remaining)))))  )  )
		
		(begin ;;;remaining is negative emit a grace
	 (insert-chord chord 0))

		
									  
									  )
									  
									  )
						      
						      (begin
							(d-WarningDialog "We have no chord"))))))))
				      
				    
				      ;;;;;  procedure "contains" decides if a note n2 belongs to a bass-note, at least in part
				      ;;;;;  if n2 starts after the bass-note ends then #f
				      ;;;;;  else if n2 ends before the bass-note ends then #t
				      ;;;;;  else if overlap of n2 with bass-note is fair proportion of n2 #t  more that one fifth, 0.2, say
				      ;;;;;  else #f
				      (define (contains bass-note n2)
				      (disp "contains " bass-note " and " n2 "\n")
					(let ((bass-note.end (+ (note.start bass-note) (note.duration bass-note)))
					      (n2.end (+ (note.start n2) (note.duration n2))))					 
					  (cond
					   ((> (note.start n2) bass-note.end)
					    #f)
					   ((> bass-note.end n2.end)
					    #t)
					   ((> (/ (- n2.end  bass-note.end) (note.duration n2)) 0.75)
					    #f)
					   (else #t))))
				      
				      
				      
;;;;;;;;;;;;;;;;;;;;;;;;; now the actual processing to loop through Notes finding an equivalent to bass-key and processing the notes belong. These are removed from Notes and then the outer loop to move on in the bass staff is taken.				      
				      
;;; first loop through Notes from the start (previous chords have been deleted) and seek the bass-note				       (disp "The notes recorded were:\n" Notes "\n")
				      (let loopgetbassnote ((index 0))
					;(disp "now index " index "bass-key " bass-key "\n")
						(set! bass-note #f)	
					(if (> (length Notes) index)
					    (if (= bass-key (note.name (car (list-ref Notes index))))
						(begin
						  (set! bass-note (list-ref Notes index))
						  (set! Notes (delq bass-note Notes))
						  )
						(loopgetbassnote (+ index 1)))))
                                           ;;;; if bass-note then that note will have been removed from Notes, next get chord notes
                                           (disp "We have bass-note " bass-note "\n")
				      (set! notes-belonging '())
				      (if bass-note					    
					  (let loop ((index 0))
					    (if (> (length Notes) index)
						(let ( (next-note (list-ref Notes index)))
						  (if (contains (car bass-note) (car next-note))
						      (begin
							(set! notes-belonging (cons*  next-note notes-belonging))
							;(append! notes-belonging next-note)
							(set! Notes (delq next-note Notes))
							(loop index)))))))
;;;;;;;;;;;;;;;;;;; finished creating notes-belonging, all these notes are now removed from Notes
				     
				      (d-PushPosition)
				      (d-MoveToStaffUp)
				      (GoToMeasureEnd)				      
				      (if (> (length notes-belonging) 0)
					  (begin	
					  (set! notes-belonging (reverse notes-belonging))
					(disp "notes-belonging looks like this: " notes-belonging "for bass dur " bass-duration "\n")
					    (insert-chords notes-belonging bass-duration)
					;(disp "insert-chords finished\n")
					)
				      
					  (begin					   
					    (eval-string (string-append  (get-durationInsert bass-duration) "(d-MoveCursorLeft)(d-StagedDelete)"))
					    
))
				      (d-PopPosition)
				      (if (d-NextObject)
					  (loopforbasskey)
					  (d-WarningDialog "finished bass staff")));;;if there is a note in the bass clef
				    (d-WarningDialog "No more bass notes")))))
		
		(begin
		  (d-WarningDialog "The Notes list is empty"))))))
    
    
    (format #t "No notes found in recording\n"))
;;;register the end of the transcription - or perhaps select everything transcribed an go through the selected bars????
    (d-PopPosition)
    ;;;;;;; now go through the staff above looking for overfull bars and remove dots on the shortest notes until it becomes the right length.


(d-PushPosition)				      
(d-MoveToStaffUp)
(define (d-NextChordInMeasure)
  (let loop ((this (d-NextObjectInMeasure)))
	(if this
	    (if (not (Music?))
		(loop)
		#t)
	    #f)))

(define (removeDot smallest-dotted)

  (apply d-GoToPosition smallest-dotted)
  (d-RemoveDot)
  (GoToMeasureBeginning))

(define (shorterDotted shortest)
  (define duration (d-GetDurationInTicks))
  (if shortest
      (if (and (positive? (d-GetDots)) (< duration shortest))
	  duration
	  shortest)
      duration))
      
(define (fixMeasure)
  (define shortest #f)
  (define smallest-dotted #f)
  (if (not (Music?))
      (d-NextChord))
  (if (Music?)
      (begin
      (let loop ()
	(set! shortest (shorterDotted shortest))
	(if (and shortest (= shortest (d-GetDurationInTicks)))
	    (set! smallest-dotted (GetPosition)))
	(if (d-NextChordInMeasure)
	    (loop)))
      (if smallest-dotted
	  (begin
	    (removeDot smallest-dotted)
	    (if (OverfullMeasure?)
		(fixMeasure)))))))
(define (fixMeasures) 
      (if (OverfullMeasure?)
	  (fixMeasure))	   	   
      (if (d-MoveToMeasureRight)
	  (fixMeasures)))
;(fixMeasures) loops infinitely?
(d-PopPosition))