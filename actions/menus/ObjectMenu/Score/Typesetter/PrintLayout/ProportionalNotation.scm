;;;ProportionalNotation
(let ((tag "ProportionalNotation")(count ProportionalNotation::params))
    (d-DirectiveDelete-score tag)
    (if count
        (set! count (number->string count))
        (set! count (d-GetUserInput (_ "Proportional Notation") (_ "Give base duration: ") "4")))
    (if (and (string? count) (string->number count))
            (ToggleDirective "score" "postfix" tag (string-append "\\set Score.proportionalNotationDuration = #(ly:make-moment 1/"  count ")\n"))
            (begin
                (d-InfoDialog (_ "Default music spacing restored.")))))
