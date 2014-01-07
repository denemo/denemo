;;;;ChordsOverBass (pure filter version)

(define-once ChordsOverBass::active #f)

(if ChordsOverBass::active
    (begin ;;;;Filter is already running stop it
      (disp "Turning off\n")
      (set! ChordsOverBass::active #f)
      (d-PutMidi 0)
      (d-SetBackground #xFFFFFF)
      )
    (begin ;;;;Filter is not already running so run it
      (set! Pitchbend::commandUp "(disp \"disabled\n\")")
      (set! Pitchbend::commandDown "(disp \"disabled\n\")")
      
      (let  ((ons '())(suspension #f)  (chord-position #f) (pedal #f))
    (define (noteOn? midi)
      (= #x90 (bit-extract midi 0 8)))
    (define (noteOff? midi)
      (= #x80 (bit-extract midi 0 8)))
    (define (pedalDown? midi)
      (and (= #xB0 (bit-extract midi 0 8))  (= (bit-extract midi 8 16) #x40) (=  (bit-extract midi 16 24) #x7F)))
    
    (define (pedalUp? midi)
      (and (= #xB0 (bit-extract midi 0 8))  (= (bit-extract midi 8 16) #x40) (=  (bit-extract midi 16 24) #x0)))
    (define (pause) (d-GetUserInput "Pausing..." "Press any key" " "))
    
;;;start on a tied note. Amalgamate the note(s) tied to with the main note
    (define (amalgamate-ties)
      (define this-dur  (string->number(car (string-split (d-GetNoteDuration) #\.))))
      (define this-dots (d-GetDots))
      (define continuing #t)
      
      (if (d-NextChordInMeasure)
          (let ((next-dur (string->number (car (string-split (d-GetNoteDuration) #\.))))
            (next-dots (d-GetDots)))
        (cond ((and (= this-dur next-dur) (= this-dots next-dots))
               (d-DeletePreviousObject)
               (d-Augment))
              
              ((and (= 1 this-dots) (= 0 next-dots) (= (/  next-dur 2) this-dur))
               (d-DeletePreviousObject)
               (d-RemoveDot)
               (d-Augment)
               (d-Augment))
              
              ((and (= (/  next-dur 2) this-dur) (= 0 this-dots) (= 0 next-dots))
               (d-DeletePreviousObject)        
               (d-Augment)
               (d-AddDot))
              (else (disp "else case\n"))))
          (if (not (d-NextChord))
          (set! continuing #f)))
      (if (and continuing (d-IsTied))
          (amalgamate-ties)))
    
;;starts on a rest, amalgamates with previous object if it is another rest
    (define (amalgamate-rests)
      (define this-dur  (string->number(car (string-split (d-GetNoteDuration) #\.))))
      (define this-dots (d-GetDots))
      
      (if (d-PrevChordInMeasure)
          (if (Rest?)
          (let ((prev-dur (string->number(car (string-split (d-GetNoteDuration) #\.))))
            (prev-dots (d-GetDots)))
            (cond
             ((and (= this-dur prev-dur) (= this-dots prev-dots))              
              (d-Augment)
              (d-NextChordInMeasure)
              (d-DeleteObject))
              ((and (= (/  this-dur 2) prev-dur) (= 0 this-dots) (= 0 prev-dots))
               (d-Augment)
               (d-AddDot)
               (d-NextChordInMeasure)
               (d-DeleteObject))
              (else
               (disp "Case not handled -did not join rests")
               (d-NextChordInMeasure))))
          (d-NextChordInMeasure))))
              
;;;;seek back to the chord started but not finished (ie before the rests)
    (define (find-suspended-chord)
      (let loop ()
        (disp "looping")
        (if (and (d-PrevChord) (Rest?))
        (loop))))
    
;;;start with cursor on the rest to be turned into a chord, end back at the same rest or rather the chord that has replaced it
    (define (continue-chord)
      (define position #f)
      (find-suspended-chord)
      (set! position (GetPosition))
      (let loop ()
        (define denemodur #f)
        (define numdots 0)
        (d-SetMark)
        (d-Copy)
        (d-ToggleTie)
        (d-NextChord)
        (set! denemodur (number->string (duration::lilypond->denemo (string->number (car (string-split (d-GetNoteDuration)  #\.))))))
        (set! numdots (d-GetDots))
        (d-Paste)
        (d-DeleteObject)
        (if (not (Appending?))
        (d-PrevChord))
        (eval-string (string-append "(d-Change" denemodur ")" (if (> numdots 0) "(d-AddDot)" "")))
        (if (d-NextChord)
        (begin
          (d-PrevChord)
          (loop))))
      (apply d-GoToPosition position)
      (amalgamate-ties))
    
    (define (add-note note)
      (set! suspension #t)
      (eval-string (string-append "(d-InsertNoteInChord \"" (d-GetNoteForMidiKey note) "\")"))  
      (PlayNote (number->string note) 400))
    
    (define (GetChords bass-key)
      (define chord-created #f)
      (define triggerPedal #f)
      (set! chord-position (GetPosition))
      (if suspension
          (continue-chord)
          (for-each add-note ons))
      (let loop ()
        (let* (
           (midi (d-GetMidi))
           (velocity (bit-extract midi 16 24))
           (note (bit-extract midi 8 16))
           (command (bit-extract midi 0 8)))  
                    ;(disp "waiting for the release of " bass-key "\n")
          (cond
           ((noteOn? midi)
        (set! ons (cons note ons));; use (null? ons) to test if empty
        (set! chord-created #t)    
        (add-note note)
        (loop))
           
           ((noteOff? midi) 
        ;(disp "A note off with chord-created=" chord-created "and suspension=" suspension "\n")
        (if (= note bass-key)               
            (begin
              (cond ((null? ons)
                 (continue-chord))
                (else
                 #t;(disp "the else case\n" ons " and rest =" (Rest?) "\n")
                 )));;;finished getting chords for this bass note
            (begin ;;; a note-off which is not the bass note
              (set! ons (delq note ons))
              (if (null? ons)
              (begin
                (set! suspension #f)
                (set! chord-created #f)
                            ;;; creating a new chord over the same note.
                (if (and pedal chord-position)
                (d-ToggleTie))
                (cond 
                 ((Rest?)
                  (d-Diminish)
                  (d-SetMark)
                  (d-Copy)
                 ;(d-DirectiveDelete-chord "ChordsOverBass")
                  (d-Paste)
                  )
                 ((> (d-GetDots) 0)
                  (d-SplitChord 3)
                  (d-MoveCursorLeft)
                  (d-MoveCursorLeft)
                  (d-MoveCursorLeft)
                  (d-Augment)
                  (d-MoveCursorRight)
                  (d-DeleteObject)
                  (ChangeToRest);on a whole chord: we remove notes all down to the Rest
                  )
                 (else
                  (d-SplitChord 2)
                  (d-MoveCursorLeft)
                  (ChangeToRest)
                  ))
                (loop))
              (loop)))))
           ((or (zero? midi) (and  (= command #xE0) (> note 32)))
        (d-SetBackground #xFFFFFF)
        (disp "Abandoning getting chord\n"))
           ((pedalUp? midi)
        (set! triggerPedal #t)
        (if (d-IsTied)
            (d-ToggleTie))

        (set! pedal #f)
        (loop))
           ((pedalDown? midi)
        (set! pedal #t)
        (loop))       
           
           (else (loop)))))
      (if (d-IsTied)
          (d-ToggleTie))
      (set! chord-position (GetPosition))) ;;; end of GetChords
    
    (define (createChordStaff)
      (d-AddBefore)
      (d-InitialClef "Treble")
      (d-StaffProperties "denemo_name=Chords"))
    
    (define (deleteToEnd)
      (d-SetMark)
      (d-GoToEnd)
      (d-Cut))
    
    (define (checkForContent)
      (if (not (None?))
          (if  (equal? (_ "y") (d-GetUserInput (_ "Non Empty Chords staff") (_ "Remove the previous transcription from this measure on?") (_ "y")))
           (deleteToEnd)
           (set! ChordsOverBass::active #f)))
      (d-MoveToStaffDown))
    
;;;;;;;; actual code
    (set! ChordsOverBass::active #t)
    
    (if  (equal? "Chords" (d-StaffProperties "query=denemo_name"))
         (set! ChordsOverBass::active (d-MoveToStaffDown)))
    
    
    (if (and ChordsOverBass::active (d-MoveToStaffUp) (equal? "Chords" (d-StaffProperties "query=denemo_name")))
        (checkForContent)
        (begin
          (if  (equal? "Chords" (d-StaffProperties "query=denemo_name"))
           (checkForContent)             
           (begin
             (d-MoveToStaffDown)
             (createChordStaff)
             (d-MoveToStaffDown)))))
    (if ChordsOverBass::active
        (let ((bass-position (GetPosition)))          
          (d-SetBackground #xB0E0B0)
          (if (not (None?))
          (let loop  ((bass-key (d-GetNoteAsMidi)))                     
            
            (d-PushPosition)
            (d-SetMark)
            (d-Copy) 
            (d-DirectivePut-chord-graphic "ChordsOverBass" "CheckMark")
             (d-DirectivePut-chord-gy "ChordsOverBass" -120)
            (if (zero? bass-key)
            (begin

;;;; if it is a rest we are copying we need to remove the tick, and set chord-position #f since we cannot tie over it
;;;; we could also flag the situation to allow the pedal to put the chord on the whole beat including this rest.
              (if (Rest?)
                  (begin
                (d-DirectiveDelete-chord "ChordsOverBass")  
                (set! chord-position #f)
                ))
              (d-MoveToStaffUp)
              (GoToMeasureEnd)
              (d-Paste))     
            (let listening ()
              (let* ((midi (d-GetMidi))
                 (velocity (bit-extract midi 16 24))
                 (note (bit-extract midi 8 16))
                 (command (bit-extract midi 0 8)))                  
                (apply d-GoToPosition bass-position)
                (cond ((and (= command #x90) (= note bass-key))
                   (if (and pedal chord-position)
                       (begin
                     (apply d-GoToPosition chord-position)
                     (d-ToggleTie)
                     (apply d-GoToPosition bass-position)))



                   (d-DirectiveDelete-chord "ChordsOverBass")              
                   ;(d-PlayMidiKey midi)
                (PlayNote (number->string note) 30 "127")

                   
                   (d-MoveToStaffUp)
                   (GoToMeasureEnd)
                   (d-Paste)
                   (d-MoveCursorLeft)
                   (if (d-IsTied)
                       (d-ToggleTie))
                   (d-StagedDelete)

                   (if (and pedal (not chord-position))
                       (amalgamate-rests))

               
                   (GetChords bass-key)
                   )
                  
                  ((= command #x90)
                   (d-PlayMidiKey midi)
                   (set! ons (cons note ons))
                   (listening)             
                   )
                  
                  ((or (zero? midi) (and  (= command #xE0) (> note 32)))
                   (disp "Finishing by abort")
                   (d-SetBackground #xFFFFFF)
                   (d-DirectiveDelete-chord "ChordsOverBass")
                   (d-MoveToMeasureLeft)
                   ;(d-PopPosition)
                  ; (d-MoveToStaffUp)
                   ;(GoToMeasureEnd)
                   ;(d-InfoDialog "Chords over Bass\nQuitting ...")             
                    ; (eval-string (string-append "(d-" (d-GetCommandFromUser) ")"))
                   (set! ChordsOverBass::active #f))
                  
                  ((= command #x80)
                   (set! ons (delq note ons))
                   (if (null? ons)
                       (set! suspension #f))
                   (listening)             
                   )
                  ((pedalUp? midi)
                   (if chord-position
                       (begin
                     (apply d-GoToPosition chord-position)
                     (if (d-IsTied) (d-ToggleTie))
                     (apply d-GoToPosition bass-position)))
                   (set! pedal #f)
                   (listening))
                  ((pedalDown? midi)
                   (set! pedal #t)
                   (listening))
                  (else
                   (d-PlayMidiKey #xF06001)
                   (disp "Ignoring " command " " note " " velocity " waiting for " bass-key "\n")
                   (listening))))))
            (if ChordsOverBass::active
            (begin 
                    ;  (pause)
             ; (d-DirectiveDelete-chord "ChordsOverBass")
              (d-PopPosition)
              ;(d-DirectiveDelete-chord "ChordsOverBass")
              
              (if (d-NextObject)
                  (begin
                (set! bass-position (GetPosition))        
                (loop (d-GetNoteAsMidi)))))))
          (disp "Finished ChordsOverBass"))))
    (set! ChordsOverBass::active #f)
    (d-SetBackground #xFFFFFF)
    (set! Pitchbend::commandUp "(d-CursorRight)")
    (set! Pitchbend::commandDown "(d-CursorLeft)"))
;;;CreateParts

(if (and (not (d-MoveToMeasureRight)) (d-GetUserInput "Chordal Accompaniment" "Mark consecutive 5ta and 8va?" "y"))
(let ()

  (define (mark-positions pos)
      (define musobj (caar (cdr pos)))
      (d-GoToPosition  #f #f (musobj.measure musobj) (musobj.horizontal musobj))
      (d-DirectivePut-chord-graphic "Consecutives" "CrossSign"))


(define (delete-lowest-notes)
  ;delete lowest note of each chord
  (d-MoveToBeginning)
  (let loop () 
    (if (d-NextChord) 
    (begin 
      (if (d-CursorToNote (d-GetNote)) (d-StagedDelete))
      (loop)))))

(define (delete-upper-notes)
  (d-MoveToBeginning)
  (let loop ((continue #f))
    (if (d-NextChord)
    (begin 
      (if (d-CursorToNote (d-GetNote 2))
          (begin 
        (d-RemoveNoteFromChord)
        (set! continue #t)
        (loop continue))
          (loop continue)))
    (if continue
        (begin
          (d-MoveToBeginning)
          (loop  #f))))))

(define (pause) (d-GetUserInput "Pausing..." "Press any key" " "))

(d-MoveToStaffUp)
(d-MoveToBeginning)
(d-GoToEnd)
(d-Copy)
(d-AddAfter)
(d-Paste)
(d-MoveToBeginning)
(d-VoicePreset1)
(d-MoveCursorLeft)
(delete-lowest-notes);;delete part 3
(delete-lowest-notes);;delete part 2

(d-AddAfter)
(d-Paste)

(d-MoveToBeginning)
(d-VoicePreset2)
(d-MoveCursorLeft)
(delete-lowest-notes);;delete part 3
(delete-upper-notes);;delete part 1 (and anything extra)

(d-AddAfter)
(d-Paste)

(d-MoveToBeginning)
(d-VoicePreset3)
(d-MoveCursorLeft)
(delete-upper-notes);;delete parts 2 and 1 (and anything extra)

(d-MoveToStaffUp)
(d-MoveToStaffUp)


(d-MoveToBeginning)
(d-PushPosition);remember which movement we were in
(disp "Pusing the right movement " (GetPosition) "?\n")
(d-SetMark)
(d-MoveToStaffDown)
(d-MoveToStaffDown)
(d-MoveToStaffDown)
(d-MoveToEnd)
(d-SetPoint)

(d-Copy)
(d-NewWindow)
(d-AddAfter)
(d-AddAfter)
(d-AddAfter)
(d-MoveToMovementBeginning)
(d-Paste)
;(pause)
(let ((movement(CreateAbstractionMovement)) (consecutives '()))
;(disp "starting \n" movement "and " consecutives "\n")
  (set! consecutives  (MapToAbstractionMovement movement AM::consecutive5th AM::consecutive8th))
;(disp "consecs 5 " consecutives " \n")
;(disp "consecs octave " consecutives " \n")
  (d-SetSaved #t)
  (d-Close)
  (d-PopPosition); return to the same movement
;(disp "Popped to the right movement?" (GetPosition) "?\n")
  (d-MoveToMovementBeginning)
  (d-DeleteAfter)
  (d-DeleteStaff)
  (d-DeleteStaff)
  (d-MoveToMovementBeginning)
  (for-each mark-positions consecutives))))))
