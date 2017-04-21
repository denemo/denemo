;;;SlurStyle
(let ((tag "SlurStyle")
    (choice (RadioBoxMenu
        (cons (_ "Dashed") "\\slurDashed")
        (cons (_ "Dotted") "\\slurDotted")   
        (cons (_ "Default") "\\slurSolid"))))
    (if choice
        (begin
            (d-SetSaved #f)
            (d-DirectivePut-chord-prefix tag (string-append "\\once" choice " "))
             (d-DirectivePut-chord-display tag (substring choice 5))
             (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX))))
