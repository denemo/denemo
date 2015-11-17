;;;FillMeasure
(let ((appending (Appending?)))
    (if (FullDurationMeasure?)
        (d-AddDuplicateMeasure)
        (if (ZeroDurationMeasure?)
            (d-PutNote #f)
            (begin
                (if appending
                    (d-MoveCursorLeft))
                (if (and (Music?) (> (d-GetDurationInTicks) 0))
                    (while (not (MeasureFillStatus)) 
                         (d-DuplicateRestNoteChord))
                    (d-PutNote #f))
                (if appending
                    (d-MoveCursorRight))))))
