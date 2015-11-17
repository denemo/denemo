;;;FillMeasure
(let ((appending (Appending?)))
    (if (FullDurationMeasure?)
        (d-AddDuplicateMeasure)
        (if (ZeroDurationMeasure?)
            (d-PutNote)
            (begin
                (if appending
                    (d-MoveCursorLeft))
                (if (and (Music?) (> (d-GetDurationInTicks) 0))
                    (while (not (MeasureFillStatus)) 
                         (d-DuplicateRestNoteChord))
                     (begin
                        (if appending
                            (d-MoveCursorRight))
                        (d-PutNote)))
                (if appending
                    (d-MoveCursorRight))))))
