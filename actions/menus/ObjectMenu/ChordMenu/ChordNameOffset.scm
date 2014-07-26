;;ChordNameOffset
(let* ((tag "ChordNameOffset") (x "-5")(y "-2")(data (d-DirectiveGet-chord-data tag)))
    (if data
        (begin
           (set! data (eval-string data))
            (set! x (car data))
            (set! y (cdr data))))
    (set! x (d-GetUserInput (_ "Chord Symbol Offset") (_ "Give horizontal displacement required") x))
    (if x
        (set! y (d-GetUserInput (_ "Chord Symbol Offset") (_ "Give vertical displacement required") y)))
    (if x
        (begin
		(d-DirectivePut-chord-data tag (format #f "(cons ~s  ~s)" x y))
		(d-DirectivePut-chord-prefix tag (string-append "\\tweak ChordName.extra-offset  #'(" x " . " y ") "))
		(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
		(d-DirectivePut-chord-display tag "<-->")))
(d-SetSaved #f))
