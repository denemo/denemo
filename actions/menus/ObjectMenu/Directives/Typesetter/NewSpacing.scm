;;;NewSpacing
(let* ((tag "NewSpacing")(params NewSpacing::params)(count (d-DirectiveGet-standalone-data tag)))
            (if (not count)
                (set! count "4"))
            (set! count (d-GetUserInput (_ "Spacing") (_ "Give new spacing: ") count))
            (if (and (string? count) (string->number count))
                    (begin
                    	(d-SetSaved #f)
                        (StandAloneDirectiveProto (cons tag (string-append "\\newSpacingSection\n\\override Score.SpacingSpanner.spacing-increment = #"  count "\n"))  #f "\n<-\nDenemo\n48")
                        (d-DirectivePut-standalone-data tag count))
                    (begin
                      (if (d-Directive-standalone? tag)
                        (d-InfoDialog (_ "To restore the prevailing music spacing delete this directive object."))
                         (d-InfoDialog (_ "Note spacing unaltered."))))))

