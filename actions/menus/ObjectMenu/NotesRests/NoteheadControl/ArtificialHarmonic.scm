;;;;;;ArtificialHarmonic
(let ((tag "ArtificialHarmonic")
        (params ArtificialHarmonic::params)
        (steps (d-GetUserInput (_ "Artificial Harmonic") (_ "Give interval above stopped note") "4")))
    (if (and steps (string->number steps))
        (begin
            (set! steps (1- (string->number steps)))
            (if (and (> steps 0) (< steps 20))
                (begin
                    (d-InsertOneNote)
                    (d-Chordize)
                    (let loop ((count steps))
                        (if (positive? count)
                            (begin
                                (d-CursorUp)
                                (loop (1- count)))))
                    (d-AddNoteToChord)
                    (d-DirectivePut-note-postfix tag "\\harmonic")
                     (d-DirectivePut-note-override tag DENEMO_OVERRIDE_GRAPHIC)
                        (d-DirectivePut-note-gx tag -10)
                    (d-DirectivePut-note-graphic tag "\n\nemmentaler\n36"))))))