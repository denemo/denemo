;;;MarkupAtEnd
(let* ((tag "MarkupAtEnd")(themarkup #f)(data (d-DirectiveGet-movementcontrol-data tag)))
(disp "eneter with " data "\n\n")
        (d-DirectivePut-movementcontrol-override tag (logior
            DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_TAGEDIT))
        (d-DirectivePut-movementcontrol-display tag (_ "Markup At End"))
        (if data
        (set! data (eval-string data))
        (set! data '()))
        
        (disp "so data is " data "\n\n")
        (set! themarkup (assq-ref data 'text))
        (if (not themarkup)
            (set! themarkup ""))
        (set! themarkup (d-GetUserInputWithSnippets (_ "Markup At End") (_ "Edit markup:") themarkup))
        (if themarkup
            (begin
                (set! data 
                    (string-append "(list (cons 'text \" "  (scheme-escape (car themarkup))  "\"))"))
                (d-DirectivePut-movementcontrol-data tag  data)
                (set! themarkup (cdr themarkup))
                (d-DirectivePut-movementcontrol-postfix tag (string-append "\\markup \\column { " themarkup " }"))
                (d-SetSaved #f))
            (d-InfoDialog "Cancelled")))
