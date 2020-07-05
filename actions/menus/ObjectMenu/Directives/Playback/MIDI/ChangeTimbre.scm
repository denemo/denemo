;;;ChangeTimbre
(let ((tag "ChangeTimbre")(prognum "46")(text "pizz."))
    (set! prognum (d-GetUserInput (_ "Change Instrument Timbre") (_ "Give MIDI program number (1-256): ") prognum))
    (if (and prognum (string->number prognum))
        (begin
            (set! prognum (number->string (1-  (string->number prognum))))
            (set! text (d-GetUserInput (_ "Change Instrument Timbre") (_ "Give accompanying text to typset here (if any): ") text))
            (if text
                (begin
                    (d-Directive-standalone tag)
                    (d-DirectivePut-standalone-postfix tag (string-append "<>^\\markup{\\italic {" text "}} "))
                    (d-DirectivePut-standalone-display tag text)
                    (d-DirectivePut-standalone-midibytes tag (string-append "0xc$ " prognum))
                    (d-DirectivePut-standalone-minpixels tag 30)
                    (d-SetSaved #f)(d-RefreshDisplay))))))

