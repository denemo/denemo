;;;CompactChordSymbol
(let ((tag "CompactChordSymbol")
    (params CompactChordSymbol::params)(symbol "9add13")(x-scale "0.4")(y-scale "0.4")(x-offset "-2")(y-offset "0"))
    (define (put-data)
        (define data (d-DirectiveGet-standalone-data tag))
        (if data
            (set! data (eval-string data))
            (set! data '()))
        (set! data (assq-set! data 'symbol symbol))
        (set! data (assq-set! data 'x-scale x-scale))
        (set! data (assq-set! data 'y-scale y-scale))
        (set! data (assq-set! data 'x-offset x-offset))
        (set! data (assq-set! data 'y-offset y-offset))
        (d-DirectivePut-standalone-data tag (format #f "'~s" data)))

    (define (put-symbol)
        (d-DirectivePut-standalone-postfix tag (string-append "_\\markup\\scale #'(" x-scale " . " y-scale") 
\\sans \\fontsize #10 { \\hspace #" (number->string (/ 5 (string->number x-scale))) " \\bold " symbol  "}"))
            (d-DirectivePut-standalone-graphic tag (string-append "\n" symbol "\nDenemo\n18"))
            (d-DirectivePut-standalone-gx tag -50)
            (d-DirectivePut-standalone-gy tag 20)
            (d-DirectivePut-standalone-minpixels tag 30)
            (put-data)
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
            (set! symbol (assq-ref data 'symbol))
            (set! x-scale (assq-ref data 'x-scale))
            (set! y-scale (assq-ref data 'y-scale))
            (set! x-offset (assq-ref data 'x-offset))
            (set! y-offset (assq-ref data 'y-offset))
            
            (set! x-offset (d-GetUserInput (_ "Chord Symbol") (_ "Give horizontal shift") x-offset))
            (if (and (string? x-offset) (string->number x-offset))
                (begin
                    (set! y-offset (d-GetUserInput (_ "Chord Symbol") (_ "Give vertical shift") y-offset))
                         (if (and (string? y-offset) (string->number y-offset))
                            (d-DirectivePut-standalone-prefix tag (string-append "-\\tweak #'extra-offset #'(" x-offset " . " y-offset ")")))))
            (get-data)
            (if symbol
                (put-symbol)))
        (if (Note?)
            (begin
                (get-data)
                (d-MoveCursorRight)
                (d-DirectivePut-standalone-prefix tag (string-append "-\\tweak #'extra-offset #'(" x-offset " . " y-offset ")"))
                (put-symbol))
            (d-WarningDialog (_ "Insert Root note of chord for symbol to attach to")))))
