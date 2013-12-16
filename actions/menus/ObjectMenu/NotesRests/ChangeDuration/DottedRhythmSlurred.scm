;;DottedRhythmSlurred
(if (Appending?)
  (let ((duration (d-GetNoteBaseDuration)) (nonprinting (MidiInput?)))
    (if duration
      (begin
        (d-AddDot)
        (d-ToggleBeginSlur)
        (eval-string (string-append "(d-" (number->string duration) ")"))
        (d-MoveCursorLeft)
        (d-Diminish)
        (d-ToggleEndSlur)
        (if (and nonprinting (not (d-GetMarkedMidiNote)))
        	(d-SetNonprinting))
        (d-MoveCursorRight)))))
