;;ChordNameOffset
(let* ((tag "ChordNameOffset") (x "2")(y "0")(data (d-DirectiveGet-chord-data tag)))
    (if data
        (begin
           (set! data (eval-string data))
            (set! x (car data))
            (set! y (cdr data))))
    (set! x (d-GetUserInput (_ "Chord Symbol Offset") (_ "Give horizontal displacement required") x))
    (if (and  x (string->number x))
        (set! y (d-GetUserInput (_ "Chord Symbol Offset") (_ "Give vertical displacement required") y)))
    (if (and  y (string->number y))
        (begin
        (d-DirectivePut-chord-data tag (format #f "(cons ~s  ~s)" x y))
        (set! y (number->string (- (string->number y)  2.5)));; -2.5 is the standard ChordName Y-Offset in ChordChartStaff.scm
        (d-DirectivePut-chord-prefix tag (string-append "\\once \\override ChordName.extra-offset =#'(" x " . " y ") "))
        (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
        (d-DirectivePut-chord-display tag "<-->")))
(d-SetSaved #f))
