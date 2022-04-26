;;;This script requires a Scheme variable DenemoMusicSignature to be set to a list of semitone steps
;;;It will test the current denemo score to see if it matches at the opening bars
;;; DenemoSearchMovement should be set to the movement to search in or 0 to search all movements
;It exits with status equal to the movement in which a match was found, or 0 if no match.
; Note that d-Quit only returns the status if invoked non-interactively, so this script cannot be tested interactively.
(disp "\nChecking score " (d-GetFilename) " movements " (if (zero? DenemoSearchMovement) "All " (number->string DenemoSearchMovement)) "\n")
(let loop ((movement_number 1))
	;(disp "Checking movement " movement_number " against " (d-GetMovementsInScore) "\n")
	(if (<= movement_number (d-GetMovementsInScore))
		(begin
		 ;(disp "testing " (zero? DenemoSearchMovement) " or " (= movement_number DenemoSearchMovement) " giving " (or (zero? DenemoSearchMovement)(= movement_number DenemoSearchMovement)) "\n\n")
			(if (or (zero? DenemoSearchMovement)(= movement_number DenemoSearchMovement))
				(begin
					(d-GoToPosition movement_number 1 1 1)
					;(disp "Went to " (GetPosition) " for " DenemoSearchMovement " at " movement_number "\n\n")
					(let ((current (d-GetNoteFromTopAsMidi))
							(sig DenemoMusicSignature)
							(test #f)
							(next #f))
						(while (and (not current) 
							(d-NextNote))
							(set! current (d-GetNoteFromTopAsMidi)))
						;(disp "starting with midi note " current " at movement " movement_number " location" (GetPosition) "\n")
						(if current
							(begin
								(while (and (d-NextNote) (not (null? sig)))
									(set! next (d-GetNoteFromTopAsMidi))
									(set! test (- next current))
									(set! current next)
									(if (= test (car sig))
										(begin  
											(set! sig (cdr sig))
											(if (null? sig)
												(begin
													;(disp  "\nThe file " (d-GetFilename) " matches the given Denemo Music Signature\n")
													(d-Quit (number->string movement_number)))))
										(begin
											(set! sig '())
											(disp "No match at location " (GetPosition) " with pattern" DenemoMusicSignature " note discrep: " test "\n\n")
											))))))))
			;(disp "Reached "	movement_number " at location " (GetPosition) "\n")			
         (loop (1+ movement_number)))
     (disp "Checked all movements")))						
(d-Quit "0")
		
            
                
        
