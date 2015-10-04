;;;FillMeasure
(let ((appending (Appending?)))
(if appending
    (d-MoveCursorLeft))
(if (and (Music?) (> (d-GetDurationInTicks) 0))
    (while (not (MeasureFillStatus)) 
         (d-DuplicateChord)))
(if appending
    (d-MoveCursorRight)))
