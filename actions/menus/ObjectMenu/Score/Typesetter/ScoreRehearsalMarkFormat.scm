;ScoreRehearsalMarkFormat
(let ((tag "ScoreRehearsalMarkFormat")
        (choice (RadioBoxMenu (cons (_ "alphabetic") "#format-mark-alphabet")
                (cons (_ "Numeric") "#format-mark-numbers")
                (cons (_ "Boxed alphabetic") "")
                (cons (_ "Boxed numeric") "#format-mark-box-numbers")
                (cons (_ "Circled alphabetic") "#format-mark-circle-alphabet")
                (cons (_ "Circled numeric") "#format-mark-circle-numbers"))))

    (if choice
        (begin
            (d-DirectivePut-score-postfix tag (string-append "\n\\set Score.markFormatter = " choice))
            (d-DirectivePut-score-override tag DENEMO_OVERRIDE_GRAPHIC)
            (d-SetSaved #f))))
