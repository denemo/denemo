;;;TypesetVersesAtEnd
(let ((theverses "") (tag "VersesAtEnd"))
    (if (d-Directive-score? tag)
        (begin
            (d-DirectiveDelete-score tag)
            (d-InfoDialog (_ "Verses will no longer be printed at the end of the piece")))
        (let loop ((num 0))
            (define theverse #f)
            (if (d-GetVerse 1)
                (begin
                    (if (zero? num) 
                        (begin
                            (set! num (d-GetUserInput (_ "Typesetting Verses at End") (_ "Which verse to start at?") "2"))
                            (if num (begin
                                        (set! num (string->number num))
                                        (if (not (d-GetVerse num))
                                            (set! num #f))))))
                    (if num
                        (begin
                            (set! theverse (d-GetVerse num))
                            (if theverse
                                (let ( (theoutput "\\markup {\\vspace #0.5 }") (thelines (string-split theverse  #\newline)))
                                    (define (output-line theline)
                                        (set! theline (string-map (lambda (c) (if (or (eq? c #\42)  (eq? c #\~)(eq? c #\_)) #\space c)) theline))
                                        (set! theoutput (string-append theoutput "\n\\markup \"" theline "\"\n")))
                    
                                    (for-each output-line thelines)
                                    (set! theverses (string-append theverses  (string-append "\n\\markup \\bold {\\vspace #3 \"\nVerse " (number->string num) "\" }\n\n" theoutput)))
                                    (loop (+ num 1))))
                            (if theverses
                                (begin
                                    (d-DirectivePut-score-postfix tag theverses)
                                    (d-DirectivePut-score-override tag DENEMO_OVERRIDE_AFFIX)
                                    (d-SetSaved #f))
                            (d-WarningDialog (_ "No verses found"))))
                        (d-InfoDialog (_ "Cancelled"))))
                  (d-WarningDialog (_ "No verses found"))))))
