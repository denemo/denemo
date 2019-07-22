;;PitchNaming
(define-once PitchNaming::active #f)
(define-once PitchNaming::explained #f)
(if (not PitchNaming::active)
        (let ((score 0)(tries 0))
            (define (noteOn? midi)
              (= #x90 (bit-extract midi 0 8)))
            (define (play-random-note max)
                    (d-GoToPosition #f #f #f (random max) )
                    (d-PlayAtCursor)(usleep 700000)
                    (d-GetNoteAsMidi))
            (if (not PitchNaming::explained)
                (begin
                    (set! PitchNaming::explained #t)
                    (d-WarningDialog (_ "Five notes are played at random from the current bar.
You must play the last one on the MIDI keyboard to score. 
You can paste as many different notes as you like into a bar -
if you include the same note many times you will get that one more often.
You can cheat by looking at the Denemo cursor position.
Invoke the command again to stop."))))
            (set! PitchNaming::active #t) 
            (while (d-NextChordInMeasure))
            (let    ((target #f)
                    (max (1+ (d-GetHorizontalPosition)))
                    (pos #f))
                (disp "Using " max " notes\n\n")       
                (d-SetMidiCapture #t)
                (let outer-loop ()
                (d-InfoDialog (string-append "Score " (number->string score) " for "  (number->string (1- tries)) " notes"))
		        (play-random-note max)(play-random-note max)(play-random-note max)
		        (set! target (play-random-note max))
                (set! tries (1+ tries))
		        (let loop ()
                    (d-InfoDialog (string-append "Score " (number->string score) " for "  (number->string (1- tries)) " notes"))
		            (let* ((midi (d-GetMidi))
		                    (note (bit-extract midi 8 16)))
		                (if (> midi 0)
		                    (begin
		                        (if (noteOn? midi)
		                            (if (= target note)
		                                (begin
		                                    (d-PlayMidiNote note 255 0 1500)
                                            (set! score (1+ score))
                                            
		                                    (usleep 1500000)
		                                    (outer-loop))
		                                (begin
		                                    (d-PlayMidiNote 78  255 9 100)
                                            (set! score (1- score))
                                            
		                                    (disp "whoops")
		                                    (loop)))
		                            (loop)))))))
                (begin
                    (d-InfoDialog (string-append "Final Score " (number->string score)  " for " (number->string (1- tries)) " notes"))
                    (disp "Bye\n\n"))))
            (begin
                (d-PutMidi 0)))
(d-SetMidiCapture #f) 
(set! PitchNaming::active #f)
