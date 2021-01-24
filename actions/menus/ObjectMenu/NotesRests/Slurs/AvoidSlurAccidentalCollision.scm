;;AvoidSlurAccidentalCollision
;;;http://code.google.com/p/lilypond/issues/detail?id=796
(let ((tag "AvoidSlurAccidentalCollision"))
    (if (d-Directive-chord? tag)
        (begin
            (d-DirectiveDelete-chord  tag)
            (d-InfoDialog (_ "Slur/Accidental avoidance removed")))
        (begin
            (if (d-IsSlurStart)
                (let ((value (d-GetUserInput "Collision Avoidance" "Give edge attraction factor for slur" "0.5")))
                    (if (and value (string->number value))
                        (begin
                            (d-DirectivePut-chord-prefix tag (string-append "\\once \\override Slur.details #'edge-attraction-factor = #" value " "))
                            (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
                            (d-DirectivePut-chord-display tag "(X"))   ; here  ) to match the string paren open
                         (d-InfoDialog (_ "Cancelled"))))   
                (d-InfoDialog (_ "Use only on a slur start to make the slur avoid accidentals on following notes")))))
    (d-RefreshDisplay)
    (d-SetSaved #f))
