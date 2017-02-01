;;;TypesetVersesAtEnd
;creates a MarkupAtEnd directive on the movement
(let* ((theverses "") (tag "MarkupAtEnd") (data (d-DirectiveGet-movementcontrol-data tag)) (choice #f))
    (if data
        (begin
            (set! choice (RadioBoxMenu (cons (_ "Append verses to current text") 'append)  (cons (_ "Edit current end-of-movement text") 'edit)  (cons (_ "Replace with verses") 'replace) (cons (_ "Cancel") 'cancel)))
            (case choice
                ((append)
                    (set! data (eval-string data))
                    (set! data (assq-ref data 'text)))
                ((edit)
                    (set! data #f)
                    (d-MarkupAtEnd))
                ((replace)
                    (set! data ""))
                ((cancel)
                    (set! data #f))
                (else (set! data #f))))
        (set! data ""))
            
    (if (and data (d-GetVerse 1))
      (begin
        (let loop ((num 0))
            (define theverse #f)
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
                            (let ((theoutput "{\\vspace #0.5 }") (thelines (string-split theverse  #\newline)))
                                (define (output-line theline)
                                    (set! theline (string-map (lambda (c) (if (or (eq? c #\\) (eq? c #\42)  (eq? c #\~)(eq? c #\_)) #\space c)) theline))
                                    (set! theoutput (string-append theoutput "\\line{" theline "}\n")))
                
                                (for-each output-line thelines)
                                (set! theverses (string-append theverses  (string-append "\n\\line\\bold {\\vspace #3 {" (_ "Verse ") (number->string num) "}}\n" theoutput)))
                                (loop (+ num 1))))))))
        (if theverses
                (begin ;(disp "Have the verses " theverses "\nAnd data " data "\n\n\n")
                    (set! theverses (string-append "\\column{" data  theverses "}"))
                    (d-DirectivePut-movementcontrol-data tag  (string-append "(list (cons 'text \"" (scheme-escape theverses) "\"))"))
                    (d-DirectivePut-movementcontrol-postfix tag (string-append "\\markup" theverses ))
                    
                    (d-SetSaved #f))
                (d-WarningDialog (_ "No verses found"))))
       (if (eq? choice 'cancel)
        (d-InfoDialog (_ "Cancelled")))))
