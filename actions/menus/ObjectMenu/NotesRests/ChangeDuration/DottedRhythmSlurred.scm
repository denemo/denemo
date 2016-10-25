;;DottedRhythmSlurred
(if (Appending?)
  (let ((duration (d-GetNoteBaseDuration)) (nonprinting (MidiInput?)))
    (if duration
      (begin
        (d-AddDot)
        (d-ToggleBeginSlur)
        (eval-string (string-append "(d-" (number->string (+ 1 duration)) ")"))
        (d-MoveCursorLeft)
        (d-ToggleEndSlur)
        (d-PlayMidiNote 53 80 9 10)
        (if (and nonprinting (not (d-GetMarkedMidiNote)))
            (d-SetNonprinting))
        (d-MoveCursorRight)))))
