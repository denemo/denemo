;;;SimulateMidi
;use the PC keyboard rows asdfghjkl;'# and qwertyuiop[] as a piano keyboard (naturals/accidentals), mimicking the effect of MIDI in. The assignments are for UK keyboard, the script needs updating to allow for re-assigning MIDI keys to PC keypresses.
(define-once SimulateMidi::active #f)
(if SimulateMidi::active
    (d-InfoDialog (_ "Press Escape in the Denemo Main Window to end MIDI keyboard simulation."))
    (let ((pedal #f))  
        (set! SimulateMidi::active #t)
        (d-InputFilterNames "Simulated MIDI Filter")
        (d-SetBackground #xFFF0D0)
        (let loop ()
            (define key 
                (if SimulateMidi::active (d-GetKeypress) "Escape"))
            (cond
                ((equal? key "a")  (d-PutMidi #xFF3C90)(loop))
                ((equal? key "w")  (d-PutMidi #xFF3D90)(loop))
                ((equal? key "s")  (d-PutMidi #xFF3E90)(loop))
                ((equal? key "e")  (d-PutMidi #xFF3F90)(loop))
                ((equal? key "d")  (d-PutMidi #xFF4090)(loop))
                ((equal? key "f")  (d-PutMidi #xFF4190)(loop));f
                ((equal? key "t")  (d-PutMidi #xFF4290)(loop))
                ((equal? key "g")  (d-PutMidi #xFF4390)(loop))
                ((equal? key "y")  (d-PutMidi #xFF4490)(loop))
                ((equal? key "h")  (d-PutMidi #xFF4590)(loop))
                ((equal? key "u")  (d-PutMidi #xFF4690)(loop))
                ((equal? key "j")  (d-PutMidi #xFF4790)(loop));b
                ((equal? key "k")  (d-PutMidi #xFF4890)(loop))
                ((equal? key "o")  (d-PutMidi #xFF4990)(loop))
                ((equal? key "l")  (d-PutMidi #xFF4A90)(loop))
                ((equal? key "p")  (d-PutMidi #xFF4B90)(loop))
                ((equal? key "semicolon")  (d-PutMidi #xFF4C90)(loop))
                ((equal? key "apostrophe")  (d-PutMidi #xFF4D90)(loop))
                ((equal? key "bracketright")  (d-PutMidi #xFF4E90)(loop))
                ((equal? key "numbersign")  (d-PutMidi #xFF4F90)(loop))

                ((equal? key "Tab") 
                    (set! pedal (not pedal))
                    (if pedal 
                        (begin
                            (d-PutMidi #x7F40B0)   
                            (d-SetBackground #xD0F0FF)      
                            (loop))
                        (begin
                            (d-PutMidi #x0040B0) 
                            (d-SetBackground #xFFF0D0)      
                            (loop))))
                
                ((not (equal? key "Escape")) 
                        (d-GetKeypress #f) ;;puts the last keypress back for normal processing
                                (loop)))
            (set! SimulateMidi::active #f)
            (d-SetBackground #xFFFFFF)
            (d-PutMidi #x0040B0) ;;pedal up
            (d-InputFilterNames "No Input Filter")
            (TimedNotice (_ "MIDI simulator end")))))
