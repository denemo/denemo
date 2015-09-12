;;;ChooseNotehead
(let ((tag "ChooseNotehead")(choice (RadioBoxMenu
    (cons (_ "Baroque") "baroque")
    (cons (_ "Neo-mensural") "neomensural")
    (cons (_ "Mensural") "mensural")
    (cons (_ "Petrucci") "petrucci")
    (cons (_ "Harmonic") "harmonic")
    (cons (_ "Harmonic Black") "harmonic-black")
    (cons (_ "Harmonic Mixed") "harmonic-mixed")
    (cons (_ "Diamond") "diamond")
    (cons (_ "Cross") "cross")
    (cons (_ "XCircle") "xcircle")
    (cons (_ "Triangle") "triangle")
    (cons (_ "Slash") "slash")
    (cons (_ "Default") "default")
    (cons (_ "AltDefault") "altdefault"))))
    (if choice
        (begin
        	(d-DirectivePut-chord-prefix tag (string-append "\\once \\override NoteHead #'style = #'" choice " "))
		(d-DirectivePut-chord-display tag (substring choice 0 3))
		(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
		(d-SetSaved #f))))
