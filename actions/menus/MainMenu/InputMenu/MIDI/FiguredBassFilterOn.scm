;;; FiguredBassFilterOn
(define-once ToggleFiguredBassMode::Active #f)
(let ((HaveTwiddle #f) (TwiddlesOn #f))
(if (not ToggleFiguredBassMode::Active)
    (let ((Figures 0)  (PedalDown #f) (Notes '()))
       (define (InsertDummyFigureIfNeeded)
            (if (d-PrevNote)
                (let ((fig (d-EditFiguredBass "query"))) (disp "fig is " fig "\n\n")
                    (if fig 
                    	(if (not (equal? fig "~"))
				(d-EditFiguredBass (string-append "figures=" fig "|0"))
				(d-NextNote))
                       (d-EditFiguredBass "figures=0"))
                        )))
       (define (AddFigure note bassnote)
                 (let ((fig (d-BassFigure bassnote note)))
                  (if (car fig);extreme interval - warn
                (d-PlayMidiNote note 255 15 350))
                  (set! Notes (cons note Notes))
                 ; (disp Notes "\n")
                  (set! Figures (cons  (cdr fig) Figures))))
                  
       (define (GetFigures)
            (if (not (null? Figures))
                (begin
                    (if (and (> (length Figures) 1) (equal? "~" (car Figures)))
                        (set! Figures (cdr Figures)))
                    (if (equal? " | " (car Figures))
                        (set! Figures (cdr Figures)))))
                (string-append "figures=" (string-join (reverse Figures))))
       (define (AddFiguresLoop bassnote lastnote)
            (let* (
                    (midi (d-GetMidi))
                    (velocity (bit-extract midi 16 24))
                    (note (bit-extract midi 8 16))
                    (command (bit-extract midi 0 8)))  
                  ;; body of the let*
                 (if (boolean? (d-GetNoteName))
                     (d-NextNote))
                 (if (or (= command #x90) (= command #x80))
                     (begin ;(disp "Start: Have Twiddele " HaveTwiddle " and TwiddlesOn " TwiddlesOn "note " note "bassnote" bassnote"\n\n")
                       (if (and (= command #x90)(= bassnote 0))
                           (begin 
                             (set! bassnote note)
                             (set! Figures '())
                             (set! Notes '())
                             (if (or (d-GetNonprinting) (= bassnote (d-GetNoteAsMidi)))
                                  (begin;; note ok
                                       (PlayNote (number->string note) 500 "127")
                                       (if (or PedalDown (positive? (d-GetKeyboardState)))
                                          (begin 
                                              (d-PlayMidiNote 60 255 9 100)
                                              (set! Figures (cons "~"  Figures))
                                              (d-EditFiguredBass (GetFigures))
                                              (set! Figures '())
                                              (set! Notes '())
                                              (set! bassnote 0)                       
                                               (if (boolean? (d-GetNoteName))
                                                  (d-NextNote)))))                  
                                  (begin                    ;; note wrong
                                    (set! bassnote 0)
                                    (d-PlayMidiNote 35 255 9 100))));;; end of  noteon while we have no bass note
                                        ;;; not attempted bass note on 
                           (begin
                             (if (and (= command #x80)(= note bassnote))
                                 (begin ;;;bass note off get the figures (moves on) and AddFiguresLoop
                                    (if TwiddlesOn
                                        (begin ;(disp "Adding a ~\n" HaveTwiddle TwiddlesOn)
                                             (set! Figures (cons "~"  Figures)))
                                        (begin
                                            (if HaveTwiddle
                                                (begin
                                                    (set! HaveTwiddle #f)
                                                    (d-SetBackground #xB0E0B0) ))))
                                             
                                   (if (not (d-SpellCheckMidiChord (cons bassnote Notes)))
                                    (d-PlayMidiNote 30 255 9 100))
                                   (set! bassnote 0)
                                   (d-EditFiguredBass (GetFigures))
                                   (if (boolean? (d-GetNoteName))
                                      (d-NextNote)));;; end of received bassnote off
                                            ;;; not bass note off
                                 (begin
                                   (if  (= command #x90)  ;;; note on
                                    (begin
                                        (if (and (= note lastnote) (> note bassnote)) ;;repeated figure, drop it and start a new group
                                            (begin ;; we have no bass note or if we do we have figures already
                                                (d-PlayMidiNote 71 255 9 100)
                                                (set! Figures (cons " | "  Figures)))
                                            (begin
                                              
                                              (if (< note bassnote)
                                                (begin
                                                    (if TwiddlesOn
                                                        (begin  ;(disp "1Have Twiddele " HaveTwiddle " and TwiddlesOn " TwiddlesOn "\n\n")
                                                            (d-SetBackground #xB0E0B0) 
                                                            (set! Figures '())
                                                            (set! HaveTwiddle #f)
                                                            (set! TwiddlesOn #f)
                                                            (d-PlayMidiNote 66 255 9 100))
                                                        (begin
                                                            (if HaveTwiddle
                                                                (begin  ;(disp "2Have Twiddele " HaveTwiddle " and TwiddlesOn " TwiddlesOn "\n\n")
                                                                    (d-PlayMidiNote 76 255 9 100)
                                                                    (d-SetBackground #xC0C0E0)      
                                                                    (set! Figures '())
                                                                    (set! HaveTwiddle #f)
                                                                    (set! TwiddlesOn #t))
                                                                (begin  ;(disp "3Have Twiddele " HaveTwiddle " and TwiddlesOn " TwiddlesOn "\n\n")
                                                                    (d-SetBackground #xB0E0B0)  
                                                                    (set! HaveTwiddle #t)
                                                                    (set! TwiddlesOn #f)
                                                                    (InsertDummyFigureIfNeeded) ;(disp "3a !!!Have Twiddele " HaveTwiddle " and TwiddlesOn " TwiddlesOn "\n\n")
                                                                    (AddFigure note bassnote)  ;;;adds a twiddle - there was no need to do this in C surely?
                                                                    (d-PlayMidiNote 46 255 9 100))))))
                                                (begin  ;(disp "4Have Twiddele " HaveTwiddle " and TwiddlesOn " TwiddlesOn "note " note "bassnote" bassnote"\n\n")
                                                    (d-SetBackground #xB0E0B0) 
                                                    (set! HaveTwiddle #f)
                                                    (set! TwiddlesOn #f)
                                                    (AddFigure note bassnote)
                                                    (PlayNote (number->string note) 500 "127")))
                                              
                                              )))))))));; end of if noteon or noteoff               
                    (begin ;not noteon/off
                        (if (and (= command #xB0) (= note #x40) (= velocity #x7F))    
                             (begin;; Pedal Down
                                (set! PedalDown #t)
                                ;(display "Pedal down")
                                (if (and (> bassnote 0) (null? Figures)) ;;; we have a bass note but no figures yet
                                    (begin 
                                      (InsertDummyFigureIfNeeded) ;check if the previous note has a figure, if not put a 0 figure to be able to continue it                   
                                      (d-PlayMidiNote 49 255 9 100)
                                      (set! Figures (cons "~"  Figures))
                                      (d-EditFiguredBass (GetFigures))
                                      (set! Figures '())
                                      (set! Notes '())
                                      (set! bassnote 0))
                                    (begin ;; we have no bass note or if we do we have figures already
                                      (d-PlayMidiNote 71 255 9 100)
                                      (set! Figures (cons " | "  Figures))))))
                        (if (and (= command #xB0) (= note #x40) (= velocity #x0))         
                          (set! PedalDown #f))
                        (if (and (= command #xB0)   (= note 1))
                             (let ((thestep  (round(/ (- velocity 64) 16))))
                                (PlayNote  
                                        (number->string  (+ 60 (* 4 thestep) ))
                                        100)         
                                (eval-string (string-append "(d-SetEnharmonicPosition " (number->string  thestep) ")"))))))
                               
                (if (= midi 0)
                    (set! ToggleFiguredBassMode::Active #f))
                 
                (if ToggleFiguredBassMode::Active 
                     (AddFiguresLoop bassnote note)))) ;;;  end of let*
        ;;;if not active procedure begins here
       (d-SetBackground #xB0E0B0)      
       (d-InputFilterNames "Figured Bass MIDI Filter")
       (set! ToggleFiguredBassMode::Active #t)
       (d-SetMidiCapture #t)       
       (AddFiguresLoop 0 0)));;; end of if not active, ends when again not active.
(d-InputFilterNames "No active MIDI Filter")
(set! ToggleFiguredBassMode::Active #f)
(d-PutMidi 0);;; to swallow up the last d-GetMidi??
(d-SetMidiCapture #f)    
(d-SetBackground #xFFFFFF))
       
