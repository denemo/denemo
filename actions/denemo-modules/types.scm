; A set of simple tests / questions for score objects. 
(define (Music?) 
  (if (string=? (d-GetType) "CHORD") #t #f))
	
(define (Note?) 
  (if (and (string=? (d-GetType) "CHORD") (d-GetNoteName)) #t #f))

(define (Rest?)
  (if (and (not (d-GetNoteName)) (string=? (d-GetType) "CHORD")) #t #f))

(define (Chord?) 
  (if (Note?)
	(if (string-contains (d-GetNotes) " ")
		#t
		#f
	)
  #f)) ; no note

(define (Singlenote?) 
  (if (Note?)
	(if (string-contains (d-GetNotes) " ")
		#f
		#t
	)
  #f)) ; no note
	
(define (Directive?) 
  (if (string=? (d-GetType) "LILYDIRECTIVE") #t #f))

(define (Timesignature?) 
  (if (string=? (d-GetType) "TIMESIG") #t #f))
  
(define (Keysignature?) 
  (if (string=? (d-GetType) "KEYSIG") #t #f))
  
(define (Clef?) 
  (if (string=? (d-GetType) "CLEF") #t #f))
  
(define (Tupletmarker?) 
  (if (or (Tupletopen?) (Tupletclose?))  #t #f))

(define (TupletOpen?) 
  (if (string=? (d-GetType) "TUPOPEN") #t #f))
  
(define (TupletClose?) 
  (if (string=? (d-GetType) "TUPCLOSE") #t #f))

(define (Tupletopen?) 
  (TupletOpen?))
  
(define (Tupletclose?) 
  (TupletClose))

(define (StemDirective?) 
  (if (string=? (d-GetType) "STEMDIRECTIVE") #t #f))  

 
(define (None?)
 (if (string=? (d-GetType) "None") #t #f))
 
(define (MeasureEmpty?) (None?)) 
	
(define (Appending?)
 (if (string=? (d-GetType) "Appending") #t #f))	 

(define (MeasureEnd?)
	(or (Appending?) (MeasureEmpty?)))

(define (MeasureBeginning?)
	(= 1 (d-GetHorizontalPosition)))
	
(define  (ColumnEmpty?)
	(define return #f)
	(define measure (d-GetMeasure)) ; to make shure we stay in the same column all the time.
	(d-PushPosition)
	(if (not (MoveToColumnStart))
	  #f ; if we have unequal staff length in staff 1 stop immediatly, 
	  (let loop ()		
		(if (and (None?) (equal? measure (d-GetMeasure)))
			(begin
				(set! return #t)
				(if (d-MoveToStaffDown)
					(loop)))		
			(set! return #f))))
	(d-PopPosition)
	return)
	
;ActionChooser is a meta function to provide a simple way to react to all types of Denemo items in the score.
(define (ActionChooser chord tupclose tupopen lilydirective clef timesig keysig stemdirective)	
	(define type (string->symbol (d-GetType)))
	(case type
	 	((CHORD) (chord))
	 	((TUPCLOSE) (tupclose))
	 	((TUPOPEN)  (tupopen))
	 	((LILYDIRECTIVE) (lilydirective))
	 	((CLEF) (clef))	
	 	((TIMESIG) (timesig))
	 	((KEYSIG) (keysig))
	 	((STEMDIRECTIVE) (stemdirective))
	 	(else #f)))

			#! (define (ActionChooserExample)
				  (ActionChooser 
					(lambda () (disp "Chord")) ;chords, notes, rests
					(lambda () (disp "tupclose")) ; tuplet close
					(lambda () (disp "Tupopen")) ; tuplet open
					(lambda () (disp "lily")) ; lilypond directive
					(lambda () (disp "clef")) ; clefs
					(lambda () (disp "time")) ; timesignatures
					(lambda () (disp "key")) ; keysignatures
					(lambda () (disp "stem")))) ; stem directives /voice presets
				(MapToSelection ActionChooserExample)!#	
