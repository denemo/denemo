;;;RevertSpacing
(let ((tag "RevertSpacing"))
    (d-SetSaved #f)
    (if (d-Directive-standalone? tag)
        (begin
            (d-DirectiveDelete-standalone tag)
            (d-NewSpacing #f))
        (begin
            (if (d-Directive-standalone? "NewSpacing")
                (d-DirectiveDelete-standalone "NewSpacing")
                (StandAloneDirectiveProto (cons tag "\\newSpacingSection\n\\revert Score.SpacingSpanner.spacing-increment\n")  #f "\n->\nDenemo\n48")))))
