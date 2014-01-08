;;;BarNumberingInterval
(let ((tag "BarNumberingInterval")(count (d-GetUserInput (_ "Regular Bar Numbering") (_ "Give interval at which to place bar numbers: ") "5")))
    (if (and (string? count) (string->number count))
            (ToggleDirective "score" "postfix" tag (string-append "\\override Score.BarNumber.break-visibility = #end-of-line-invisible
                \\set Score.barNumberVisibility = #(every-nth-bar-number-visible " count ") "))
            (begin
                (d-DirectiveDelete-score tag)
                (d-InfoDialog (_ "Default bar numbering restored")))))
