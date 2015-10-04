;;;FillMeasure
(let ((appending (Appending?)))
(if appending
    (d-MoveCursorLeft))
(if (Music?)
    (while (not (MeasureFillStatus)) 
         (d-DuplicateChord)))
(if appending
    (d-MoveCursorRight)))
