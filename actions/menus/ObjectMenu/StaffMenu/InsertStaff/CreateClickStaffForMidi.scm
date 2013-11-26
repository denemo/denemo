;;;CreateClickTrackForMidi
(let ()
 (define (writeBar numerator denominator)
    (let loop ((count numerator))
      (if (positive? count)
	  (begin
	    (eval-string (string-append "(d-" (number->string (duration::lilypond->denemo denominator)) ")"))
	    (loop (- count 1))))))
	
 (define (writeAllBars duration tempo)	
	(if (and duration tempo)
		(let ((numer (list-ref tempo 1))  (denom (list-ref tempo 2)) (spqn (list-ref tempo 3)) )
			(define seconds_per_bar (* spqn (* numer (* (/ 4 denom)))))
			(define bars (round (/ duration seconds_per_bar)))
			(define thetimesig (string-append (number->string numer) "/" (number->string denom)))
			(if (not (None?))
							(d-AddMeasure))
			(if (not (equal? (d-GetPrevailingTimesig) thetimesig))
				(d-InsertTimeSig thetimesig))
			(let loop ((count bars))
				(if (positive? count)
					(begin					
						(writeBar numer denom)
						(loop (1- count))))))))
						
	(define duration #f)
	(define old-time 0)
	(define tempo #f)    
	(define old_highlight (d-HighlightCursor #f))
	(define next-tempo #f)
	
	;;; the procedure	
	(let loop ((count 0))
		(set! tempo (d-GetRecordedMidiTempo count))
		(set! next-tempo #f)
		(if tempo
			(begin
				(set! next-tempo (d-GetRecordedMidiTempo (1+ count)))
				(if next-tempo
					(begin
						(set! duration (- (list-ref next-tempo 0) old-time))
						(set! old-time  (list-ref next-tempo 0)))
					(begin
						(set! duration (- (d-GetRecordedMidiDuration) old-time)))))		
			(begin
				(set! duration (d-GetRecordedMidiDuration))))
		(writeAllBars duration tempo)
		(if next-tempo				
			(loop (1+ count))))
	(d-HighlightCursor old_highlight))

