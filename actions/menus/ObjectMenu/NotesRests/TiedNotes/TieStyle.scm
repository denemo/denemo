;;;TieStyle
(let ((tag "TieStyle")
    (choice (RadioBoxMenu
        (cons (_ "Dashed") "\\tieDashed")
        (cons (_ "Dotted") "\\tieDotted")   
        (cons (_ "Default") "\\tieSolid"))))
    (if choice
        (begin
            (d-SetSaved #f)
            (d-DirectivePut-chord-prefix tag (string-append "\\once" choice " "))
             (d-DirectivePut-chord-display tag (substring choice 4))
             (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX))))
