;;;FillMeasure
(let ((params FillMeasure::params)(appending (Appending?))(denominator (cadr (string-split (d-GetPrevailingTimesig) #\/))))
    (if (FullDurationMeasure?)
        (d-AddDuplicateMeasure (if params 'play #f))
        (if (ZeroDurationMeasure?)
            (eval-string (string-append "(d-" (number->string (duration::lilypond->denemo (string->number denominator))) ")"))
            (begin
                (if appending
                    (d-MoveCursorLeft))
                (if (and (Music?) (> (d-GetDurationInTicks) 0))
                    (while (not (MeasureFillStatus)) 
                         (d-DuplicateRestNoteChord))
                    (d-PutNote #f))
                (if appending
                    (d-MoveCursorRight))))))
;;;;;;;;;