;;;NewSpacing
(let ((tag "NewSpacing")(count NewSpacing::params))
    (if (d-Directive-standalone? tag)
        (begin
            (d-InfoDialog (_ "Reverting to the default spacing"))
            (d-DirectiveDelete-standalone tag)
            (StandAloneDirectiveProto (cons tag "\\newSpacingSection\n\\revert Score.SpacingSpanner.spacing-increment\n")  #f "\n->\nDenemo\n48"))
        (begin
            (if count
                (set! count (number->string count))
                (set! count (d-GetUserInput (_ "Spacing") (_ "Give new spacing: ") "4")))
            (if (and (string? count) (string->number count))
                    (StandAloneDirectiveProto (cons tag (string-append "\\newSpacingSection\n\\override Score.SpacingSpanner.spacing-increment = #"  count "\n"))  #f "\n<-\nDenemo\n48")
                    (begin
                        (d-InfoDialog (_ "Prevailing music spacing restored.")))))))
