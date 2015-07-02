;;;MarkupAtEnd
(let* ((tag "MarkupAtEnd")(themarkup #f)(data (d-DirectiveGet-movementcontrol-data tag)))
        (d-DirectivePut-movementcontrol-override tag (logior
            DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_TAGEDIT))
        (d-DirectivePut-movementcontrol-display tag (_ "Markup At End"))
        (if data
        (set! data (eval-string data))
        (set! data '()))
        (set! themarkup (assq-ref data 'text))
        (if (not themarkup)
            (set! themarkup ""))
        (set! themarkup (d-GetUserInputWithSnippets (_ "Markup At End") (_ "Edit markup:") themarkup))
        (if themarkup
            (begin
                (set! data (assq-set! data 'text (car themarkup)))
                (d-DirectivePut-movementcontrol-data tag (format #f "'~s" data))
                (set! themarkup (cdr themarkup))
                (d-DirectivePut-movementcontrol-postfix tag (string-append "\\markup " themarkup))
                (d-SetSaved #f))
            (d-InfoDialog "Cancelled")))
