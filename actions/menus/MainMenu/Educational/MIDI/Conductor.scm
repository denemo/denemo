;;;;;;;;;;;;
;;The Conductor

    (let 
	( (loop 0) (show-graphic 0) (graphic "One"))
	 (begin

	   (set! show-graphic (lambda (count)
				(cond ((zero? count)
				       (set! graphic "LargeZero"))
				      ((= 1 count)
				       (set! graphic "LargeOne"))
				      ((= 2 count)
				       (set! graphic "LargeTwo"))
				      ((= 3 count)
				       (set! graphic "LargeThree"))
				      ((= 4 count)
				       (set! graphic "LargeFour"))
				      ((= 5 count)
				       (set! graphic "LargeFive"))
				      ((= 6 count)
				       (set! graphic "LargeSix"))
				      ((= 7 count)
				       (set! graphic "LargeSeven"))
				      ((= 8 count)
				       (set! graphic "LargeEight"))
				      ((= 9 count)
				       (set! graphic "LargeNine")))
					      
				(d-DirectivePut-standalone-graphic "Conductor" graphic)
				(d-DirectivePut-standalone-minpixels "Conductor" (d-DirectiveGet-standalone-width "Conductor"))
				;;(display  (d-DirectiveGet-standalone-width "Conductor"))
			
            ))

	   (set! loop  (lambda (measure)
			 (let* ( 
				(oldtens 0)
				(oldunits 0)
				(tens 0)
				(units 0)
				(beat 0)
				(graphic "One")
				(midi (d-GetMidi))
				(velocity (bit-extract midi 16 24))
				(note (bit-extract midi 8 16))
				(command (bit-extract midi 0 8)))
			   ;; body of the let*
			       (begin
				 ;;(display command)
;; (display note) (display velocity)

			 (if (and (= command #xB0) (= note #x40) (= velocity #x7F))
				     (begin
				       	;;(display "Pedal down")
					(set! measure (1+ measure))
					;;(display measure)
					(if (> measure 99) 
					    (begin
					      (set! measure 0)
					    (set! tens 0)
					    (set! oldtens -1)
					    ))
					(set! tens (floor (/ measure 10)))
					(if (> tens oldtens)
					    (begin
					      (set! oldtens tens)
                                              ;;; step to previous measure and show tens
					      (d-MoveToMeasureLeft)
					      (show-graphic tens)
					      (d-MoveToMeasureRight)
					      );;end begin
					    );;end if
                                        (set! units (- measure (* oldtens 10)))
                                        (show-graphic units)
					(d-RefreshDisplay)
					))
					
				 (if (not (= command 0))
				     (loop measure))
				 ));; end of let, the body of the loop proc
			   )) ;;; loop definition

(d-InsertMeasure)
(d-InsertMeasure)
(d-MoveToMeasureLeft)
	   (loop 0)
	  ))
