;;ToggleNoteTie
(let ((tag "Tie"))
    (if (Chord?)
        (begin
            (if (d-Directive-note? tag)
                (d-DirectiveDelete-note tag)
                (begin
                    (d-DirectivePut-note-postfix tag "~")
                    ;;for some reason we do not have ⏝ in the font
                    (d-DirectivePut-note-graphic tag "\n~
                    Denemo
                    30")))
            (d-RefreshDisplay)
            (d-SetSaved #f))
        (d-WarningDialog (_ "Individual Ties only apply to notes within chords."))))
