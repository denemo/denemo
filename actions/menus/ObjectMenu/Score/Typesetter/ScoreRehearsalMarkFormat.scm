;ScoreRehearsalMarkFormat
(let ((tag "ScoreRehearsalMarkFormat")
        (choice (RadioBoxMenu
                (cons (_ "Default") "#format-mark-letters")
                (cons (_ "Alphabetic") "#format-mark-alphabet")
                (cons (_ "Numeric") "#format-mark-numbers")
                (cons (_ "Boxed alphabetic") "#format-mark-box-alphabet")
                (cons (_ "Boxed numeric") "#format-mark-box-numbers")
                (cons (_ "Circled alphabetic") "#format-mark-circle-alphabet")
                (cons (_ "Circled numeric") "#format-mark-circle-numbers"))))

    (if choice
        (begin
            (d-DirectivePut-score-postfix tag (string-append "\n\\set Score.markFormatter = " choice))
            (d-DirectivePut-score-override tag DENEMO_OVERRIDE_GRAPHIC)
            (d-SetSaved #f))))
