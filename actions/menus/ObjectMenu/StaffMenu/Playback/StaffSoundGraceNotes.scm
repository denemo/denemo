;;;StaffSoundGraceNotes
(let ()
    (define (do-one)
        (let ((count 0)(dur #f)(dur1 #f)(dur2 #f)(pos (GetPosition)))
            (while (and (d-IsGrace) (d-MoveCursorRight))
                (set! count (1+ count)))
            (if (and (positive? count) (not (Appending?)) (Note?) (not (d-IsGrace)))
                (begin
                    (set! dur (d-GetDurationInTicks))
                    (if (positive? (d-GetDots))
                            (set! dur1 (* (/ dur 3) 2))
                            (set! dur1 (/ dur  2)))

                    (set! dur1 (round (/ dur1 count)))
                    (set! dur2 (- dur (* count dur1)))
              ;  (disp "count " count " pos " pos "main note at " (GetPosition) "dur =" dur  " dur1=" dur1 "dur2=" dur2 "\n")
                (apply d-GoToPosition pos)
                (let loop ((num (1- count)))
                        (d-DirectivePut-chord-prefix "Duration" (d-GetNoteDuration))
                        (d-DirectivePut-chord-override "Duration" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
                        (d-SetDurationInTicks dur1)
                        (d-MoveCursorRight)
                        (if (positive? num)
                            (loop (1- num))))
                (d-DirectivePut-chord-prefix "Duration" (d-GetNoteDuration))
                (d-DirectivePut-chord-override "Duration" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
                (d-SetDurationInTicks dur2)
                (d-DirectivePut-chord-override "Duration" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
                (d-SetSaved #f)))))
    
  (d-MoveToBeginning)
  (let loop ()
    (if (d-IsGrace)
        (do-one))
    (while (and (d-MoveCursorRight) (not (d-IsGrace))))
    (if (d-IsGrace)
        (loop))))