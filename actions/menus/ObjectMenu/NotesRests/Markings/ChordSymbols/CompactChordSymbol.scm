;;;CompactChordSymbol
(let ((tag "CompactChordSymbol")
    (params CompactChordSymbol::params)(symbol "9add13")(x-scale "0.4")(y-scale "0.4"))
    (define (put-symbol)
        (d-DirectivePut-standalone-postfix tag (string-append "_\\markup\\scale #'(" x-scale " . " y-scale") 
\\sans \\fontsize #10 { \\hspace #" (number->string (/ 5 (string->number x-scale))) " \\bold " symbol  "}"))
            (d-DirectivePut-standalone-graphic tag (string-append "\n" symbol "\nDenemo\n18"))
            (d-DirectivePut-standalone-gx tag -50)
            (d-DirectivePut-standalone-gy tag 20)
            (d-DirectivePut-standalone-minpixels tag 30)
            (d-DirectivePut-standalone-data tag (format #f "(list ~s ~s ~s)" symbol x-scale y-scale))
            (d-SetSaved #f)
            (d-RefreshDisplay))
    (define (get-data)
        (set! symbol (d-GetUserInput (_ "Chord Symbol") (_ "Give text of chord symbol") symbol))
        (if symbol
            (begin
                (set! x-scale (d-GetUserInput (_ "Chord Symbol") (_ "Give horizontal scale") x-scale))
                (set! y-scale (d-GetUserInput (_ "Chord Symbol") (_ "Give vertical scale") y-scale)))))
    
    
    (if (d-Directive-standalone? tag)
        (let ((data (eval-string (d-DirectiveGet-standalone-data tag))))
            (set! symbol (list-ref data 0))
            (set! x-scale (list-ref data 1))
            (set! y-scale (list-ref data 2))
            
                (let ((x-off "-1.5")(y-off "0"))
                    (set! x-off (d-GetUserInput (_ "Chord Symbol") (_ "Give horizontal shift") x-off))
                    (set! y-off (d-GetUserInput (_ "Chord Symbol") (_ "Give vertical shift") y-off))
                    (d-DirectivePut-standalone-prefix tag (string-append "-\\tweak #'extra-offset #'(" x-off " . " y-off ")")))
            (get-data)
            (if symbol
                (put-symbol)))
        (if (Note?)
            (begin
                (get-data)
                (d-MoveCursorRight)
                (put-symbol))
            (d-WarningDialog (_ "Insert Root note of chord for symbol to attach to")))))