;;;;;;;;;;Anacrusis-amended with DW's Rebar functions
(let ((duration 0)(TupletScaleFactor 1))
	(define (GetNoteBeat )	;get duration of a note as a fraction of a whole note, e.g. dotted quarter = 3/8
		(let ((note 0) (len 0 ) (DotIndex 0) (NumDots 0) (NoteBeat 0))
			(begin		
			(if (equal? (d-GetType) "TUPOPEN") ;FIXME-no support for nested tuplets.
				(set! TupletScaleFactor (string->number (d-GetTuplet))))
			(if (equal? (d-GetType) "TUPCLOSE") (set! TupletScaleFactor 1))
			(if (equal? (d-GetType) "CHORD" ) 
				(if (not (d-ToggleGrace "query="))	;if it's not a grace, continue; otherwise, leave it as 0.
					(begin
						(set! note (d-GetNoteDuration))
						(set! len (string-length note) )
						(set! DotIndex (string-index note #\.) )
						( if DotIndex (begin		;if DotIndex is a valid number...
							(set! NumDots (- len DotIndex) )
							(set! note (substring note 0 DotIndex) )  ;trim off dots
						) )
						(set! note (string->number note))
						(set! NoteBeat ( / 1 note))
						;now modify base NoteBeat by (2-2^(-NumDots))
						(set! NoteBeat (* NoteBeat (- 2 (expt 2 (* -1 NumDots)))))				
					)
				)
			)
			(* TupletScaleFactor NoteBeat)	;return NoteBeat--modified by TupletScale Factor.
			)
		)
	);GetNoteBeat
	(define (LoopThroughBar)   ;stops once we've run out of new notes.
		(if (d-NextObjectInMeasure)	;as long as there's more stuff to process...
			(begin
				(set! duration (+ duration (GetNoteBeat)) )	;we increment the measure's duration,
				(LoopThroughBar)	;and keep going until done with the bar.				
			)
		)
	)

	(d-MoveToBeginning)
	(set! duration (GetNoteBeat))
	(LoopThroughBar)	;run through bar adding up all durations.
	(d-MoveToBeginning)
	(set! duration (* duration 128))	;number of 128ths in duration
	(if (equal? 0 duration)
		(begin
			(d-WarningDialog (_ "Put notes/rests in the first measure for the upbeat required\nThen issue this command")))
		(if (integer? duration)
			(begin
	                        (d-DirectivePut-standalone "Anacrusis")
				(d-DirectivePut-standalone-postfix "Anacrusis" (string-append "\\partial 128*" (number->string duration) " " ))
				(d-DirectivePut-standalone-override "Anacrusis" DENEMO_OVERRIDE_DYNAMIC) ;This allows scripts that notice it to re-run the command to refresh the anacrusis
				(d-DirectivePut-standalone-minpixels "Anacrusis" 60)
				(d-DirectivePut-standalone-display "Anacrusis" (_ "Upbeat"))
				(d-DirectivePut-standalone-ty "Anacrusis" 50)
				(d-RefreshDisplay)
			)
			(d-WarningDialog (_ "This measure's duration is too complex for this script.\nSimplify or insert the required lilypond manually."))
		)
	)
	(if (not (d-MoveToMeasureRight))
	         (begin
	        (d-AppendMeasure)
	
	    (d-MoveToMeasureRight))))
