;;;BarNumberingAlignment
(let ((tag "BarNumberingAlignment")(choice (RadioBoxMenu (cons  (_ "Center on barline") "CENTER") (cons (_ "Align to left") "LEFT"))))
    (if  (string? choice) 
            (ToggleDirective "score" "postfix" tag (string-append "\\override Score.BarNumber.self-alignment-X = #" choice " "))
            (begin
                (d-DirectiveDelete-score tag)
                (d-InfoDialog (_ "Default bar number alignment restored")))))
