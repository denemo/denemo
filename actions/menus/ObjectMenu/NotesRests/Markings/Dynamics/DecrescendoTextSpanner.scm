;;;DecrescendoTextSpanner
(if (Music?)
    (let ((tag "DecrescendoTextSpanner")(text #f))
    (if (d-Directive-chord? tag)
        (d-DirectiveDelete-chord tag)
        (begin
            (if (and (string? DecrescendoTextSpanner::params) (not (equal? DecrescendoTextSpanner::params "edit")))
                (set! text DecrescendoTextSpanner::params)
                (set! text (d-GetUserInput (_ "Decrescendo Text Spanner") (_ "Give text ") "poco")))
            (if text
                (begin
                    (d-DirectivePut-chord-prefix tag  (string-append
                        "\\set decrescendoText = \\markup {\\italic { " text "}}\\set decrescendoSpanner = #'text "))

                    (d-DirectivePut-chord-postfix tag "\\>")
                    (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
                    (d-DirectivePut-chord-display tag  text)))))
    (d-SetSaved #f)))
