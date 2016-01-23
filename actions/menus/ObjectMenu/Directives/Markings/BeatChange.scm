;;;BeatChange
(let ((tag "BeatChange")(i 0)(Beat1 #f) (Beat2 #f) (LilyString #f) (DisplayString #f) (Input #f) (NewBPM 100) (ValidBPM? #f)(MidiBPM 100) (initial-text "(") (final-text ")"))

;GetBPM:
;sets MidiBPM, ValidBPM? based on user input.  This will allow denemo's midi to respect the beat change.
(define (GetBPM BaseBeat )      
    (let ((dotted #f)(defaultBPM "60") (len 1)(duration 1)(bpm 60))
        
        ;if BaseBeat is not specified, must query the user as to what should be the first beat unit:
        (if (equal? "" BaseBeat)  (set! BaseBeat (d-GetUserInput (_ "Metronome Marking") (_ "Give unit beat duration (e.g., 4. for 𝅘𝅥. ) \n or enter n for none:") "4" )))
        
        (if (or (equal? BaseBeat (_ "n") ) (equal? #f BaseBeat))
        
            ;if user doesn't want to use BPM.
            (set! ValidBPM? #f) 
            
            ; if user wants to use bpm...
            (begin              
                (set! len (string-length BaseBeat) ) 
                (set! dotted (equal? "." (substring BaseBeat (- len 1) len ))  ) ;see if a dot at end
                (if dotted
                    (set! duration (substring BaseBeat 0 (- len 1)))  ;if there's a dot, cut it off from BaseBeat to get base duration.
                    (set! duration BaseBeat)  
                )           
                
                (set! bpm (d-GetUserInput (_ "Metronome Marking")  (string-append (_ "Give number of these ") BaseBeat (_ " beats per minute:")) defaultBPM ) )
                
                
                (set! ValidBPM?   (not (equal? (and (string->number duration) (string->number bpm) ) #f)))  ;don't go unless both are numbers.
                
                ;don't go unless base duration is valid lilypond: (could go higher if wanted):
                (set! ValidBPM? (and ValidBPM? (or   (equal? duration "1")(equal? duration "2")(equal? duration "4")(equal? duration "8")
                    (equal? duration "16")) ) )
                (if (and bpm  (equal? ValidBPM? #t))
                    (if dotted (set! MidiBPM (number->string (floor (* (/ (string->number bpm) (string->number duration)) 6 ) ) ) ) 
                        (set! MidiBPM (number->string (floor (* (/ (string->number bpm) (string->number duration) ) 4))  ) )
                    ) ;want * 3/2 for dotted,*4 since midi uses quarters and divide by duration,     
                    (d-WarningDialog "Incorrect BPM syntax.")
                )
            )
        )
    );let
    );define GetBPM
    
    ;Begin of main function
    
    ;Input from the user what kind of beat change they want:
    (set! Input (d-GetUserInput (_ "Beat Change") 
        (string-append (_ "Enter beat change as [beat1]=[beat2] with the beats in LilyPond syntax,")
        (_ "\ni.e. 4 for 𝅘𝅥, 8. for dotted 𝅘𝅥𝅮, etc.")
        (_ "\nPlace immediately after a barline to center the beat change over the barline."))
     "4=4."))
    ;Input from the user text to be placed before
    (set! initial-text (d-GetUserInput (_ "Beat Change") 
         (_ "Enter text (if any) to appear before the [beat1]=[beat2]") initial-text))
    (set! final-text (d-GetUserInput (_ "Beat Change") 
         (_ "Enter text (if any) to appear after the [beat1]=[beat2]") final-text))
    (if (not initial-text)
        (set! initial-text ""))
    (if (not final-text)
        (set! final-text ""))
    ;remove spaces from Input:      
    (set! DisplayString Input)
    (set! i (string-index Input #\=))
    (set! Beat1 (substring Input 0 i))
    (set! Beat2 (substring Input (+ 1 i)))
    
    ;now we want to get the new bpm from the user.  Because e.g., if we had 4=100, and now 4. is the beat, we want the midi to be at a bpm of 4=150.  
    ;This could be done automatically by a smart script that learned the current BPM and modified it appropriately.
    (set! MidiBPM (GetBPM Beat2))
    
    ;set the string that tells lilypond how to make a beat change:
    (set! LilyString (string-append "\\mark \\markup \\tiny {" initial-text " \\general-align #Y #DOWN \\note #\"" Beat1
         "\" #UP = \\general-align #Y #DOWN \\note #\"" Beat2 "\" #UP "
        final-text " } "))
            
    ;now put in all the info into the denemo directive, including a midi bpm change if we have a valid one:
    (if (not (d-Directive-standalone? tag))
        (d-DirectivePut-standalone tag))
    (d-DirectivePut-standalone-postfix tag LilyString)
    (d-DirectivePut-standalone-display tag DisplayString)
    (d-DirectivePut-standalone-minpixels tag 15)
    (if (equal? ValidBPM? #t)
        (begin 
            (d-DirectivePut-standalone-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_TEMPO DENEMO_OVERRIDE_STEP))
            (d-DirectivePut-standalone-midibytes tag MidiBPM)
        )
    )
    (d-SetSaved #f)
    (d-RefreshDisplay)      
)
