;;;DecrescendoTextSpanner
(if (Music?)
    (let ((tag "DecrescendoTextSpanner")(params DecrescendoTextSpanner::params)(text (_ "dim. (or poco if continuing the dim.)")))
    (if (equal? params "edit")
            (set! params (RadioBoxMenu (cons (_ "Edit") 'edit)  (cons (_ "Delete") 'delete) (cons (_ "Advanced") 'advanced))))    
 
    (if (d-Directive-chord? tag)
          (cond 
            ((not params)
                (set! params 'delete))
            ((list? params)
                            (begin
                                (d-WarningDialog (_ "Sorry, this operation needs to be applied to the start of the Decrescendo."))
                                (set! params 'abandon)))
            ((string? params)
                        (set! text params))
            ((eq? params 'advanced)
                    (if (not (d-DirectiveTextEdit-chord tag))
                        (set! params 'delete)))
            (else
                    (set! text (d-DirectiveGet-chord-display tag)))))
     (if (not (eq? params 'abandon))   
        (begin
            (if (eq? params 'delete)
                 (begin
                    (d-DirectiveDelete-chord tag)
                    (d-InfoDialog (_ "Deleted")))        
                (begin
                    (if (and (string? params) (not (equal? params "edit")))
                        (set! text params)
                        (set! text (d-GetUserInput (_ "Decrescendo Text Spanner") (_ "Give text for start or continuation") text)))
                    (if text
                        (begin
                            (d-DirectivePut-chord-prefix tag  (string-append
                                "\\once\\set decrescendoText = \\markup {\\italic { " text "}}\\once\\set decrescendoSpanner = #'text "))

                            (d-DirectivePut-chord-postfix tag "\\>")
                            (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
                            (d-DirectivePut-chord-display tag  text)))))
            (d-SetSaved #f)))))


