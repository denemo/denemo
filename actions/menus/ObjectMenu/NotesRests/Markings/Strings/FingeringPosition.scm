;;;FingeringPosition
(let ((fingering-tag "Fingering"))  
    (define finger #f)
    (define (get-direction text)
        (if text
            text
            "up"))

   ;puts posn as data field of note directive
    (define (do-position posn)
        (d-DirectivePut-note-data fingering-tag  posn))
        
        ;;;gets the position from the data field of the note
    (define (get-position)
        (define posn (d-DirectiveGet-note-data fingering-tag))
            (if posn
                posn
                "up"))
        
    (define (do-up)
                (do-position "up"))
    (define (do-down)
                (do-position "down"))
    (define (do-left)
                (do-position "left"))
    (define (do-right)
                (do-position "right"))
                
    (define (do-finger-positions)
                (define tag "FingeringPositions")
                (define directions "")
                (d-Chordize #t)

                (let loop ((count 1))
                        (if (d-CursorToNote (d-GetNote count))
                            (begin
                                (set! directions (string-append directions " " (get-direction (get-position))))
                                (loop (+ 1 count)))))
                (d-DirectivePut-chord-prefix tag (string-append "\\set fingeringOrientations = #'(" directions ") "))
                (d-DirectivePut-chord-override tag (logior DENEMO_OVERRIDE_DYNAMIC DENEMO_OVERRIDE_AFFIX))
                (d-RefreshDisplay)
                (d-SetSaved #f))        
                
    (define value #f)
            
    (set! value (d-PopupMenu (list (cons (cons (_ "Above") (_ "Places the fingering above the chord")) do-up)
                                                                            (cons (cons (_ "Below") (_ "Places the fingering below the chord")) do-down)
                                                                            (cons (cons (_ "Left") (_ "Places the fingering to the left of the chord")) do-left)
                                                                            (cons (cons (_ "Right") (_ "Places the fingering to the right of the chord")) do-right))))  
    (if value
            (let ((initial (d-GetCursorNoteWithOctave)))
                (value)
                (do-finger-positions)
                (d-CursorToNote initial)
                )))
