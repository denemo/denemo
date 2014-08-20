;;;AccidentalStyle
(let ((tag "AccidentalStyle")(style #f) (params AccidentalStyle::params))
    (if (equal? params "edit")
        (set! params #f))
    (if (string? params)
        (set! style params)
        (set! style 
            (RadioBoxMenu (cons (_ "Default") "default")
                            (cons (_  "Voice") "voice")
                            (cons (_  "Modern") "modern")
                            (cons (_  "Modern Cautionary") "modern-cautionary")
                            (cons (_  "Modern Voice") "modern-voice")
                            (cons (_  "Piano") "piano")
                            (cons (_  "Piano Cautionary") "piano-cautionary")
                            (cons (_  "Neo-Modern") "neo-modern")
                            (cons (_  "Neo-Modern Cautionary") "neo-modern-cautionary")
                            (cons (_  "Neo-Modern Voice Cautionary") "neo-modern-voice-cautionary")
                            (cons (_  "Dodecaphonic") "dodecaphonic")
                            (cons (_  "Teaching") "teaching")
                            (cons (_  "No Reset") "no-reset")
                            (cons (_  "Forget") "forget"))))
(if style
    (begin
        (d-SetSaved #f)
        (d-DirectivePut-score-prefix tag  (string-append "
      \\layout{\\context {
        \\Score
        \\accidentalStyle " style "
      }}
"))
        (d-DirectivePut-score-display tag style))))