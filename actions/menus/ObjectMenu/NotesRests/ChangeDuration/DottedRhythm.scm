;;DottedRhythm
(if (Appending?)
  (let ((duration (d-GetNoteBaseDuration)) (nonprinting (MidiInput?)))
    (if duration
      (begin
        (d-AddDot)
        (eval-string (string-append "(d-" (number->string (+ 1 duration)) ")"))
        (d-MoveCursorLeft)
        (if (and nonprinting (not (d-GetMarkedMidiNote)))
            (d-SetNonprinting))
        (d-MoveCursorRight)))))
