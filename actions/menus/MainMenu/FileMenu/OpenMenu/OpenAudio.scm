;;;;;;;;;;;;;;OpenAudio
(d-PushPosition)
(let ((seconds (d-OpenSourceAudioFile)))
	(if (and seconds (EmptyMeasure?) (not (d-MoveToMeasureRight)) (not (d-MoveToStaffDown)))
		(let ( (timesig (d-InitialTimeSig "query=timesigname")) (numerator #f)(denominator #f))
			(set! numerator (string->number (car (string-split   timesig #\/))))
			(set! denominator (string->number (cadr (string-split  timesig #\/))))
			
			(let loop ((count numerator))
	 			(if (> count 0)
	 				(begin
	 					(eval-string (string-append "(d-" (number->string (duration::lilypond->denemo denominator)) ")")) 
	 					(d-CursorUp)
	 					(loop (- count 1)))))
	 					
	 		(d-RecreateTimebase)
	 					
	 		(let loop ((count (/ seconds (d-GetMidiOffTime)))) 
				(if (> count 0)
					(begin 
						(d-AddDuplicateMeasure)
						(loop (- count 1)))))
			(d-SetPlaybackInterval 0.0 seconds))))
						
(d-PopPosition)	