;TextSpanStart
(if (Music?)
    (let ((tag "TextSpanStart")(text "rall.")(params TextSpanStart::params) (direction ""))
        (if (equal? params "edit")
            (set! params (RadioBoxMenu (cons (_ "Edit") 'edit)  (cons (_ "Delete") 'delete) (cons (_ "Advanced") 'advanced))))
        (if (d-Directive-chord? tag)
          (cond 
            ((not params)
                (set! params 'delete))
            ((list? params)
                (let ((type (car (list-ref params 0)))   (params (cdr (list-ref params 0))))
                    (if (eq? type 'direction)
                        (set! direction params)
                        (begin
                            (set! direction #f)
                            (d-WarningDialog  (_ "Sorry, not yet implemented"))))))
            ((string? params)
                        (set! text params))
            ((eq? params 'edit)
                (set! direction (GetLilyPondDirection)))
            ((eq? params 'advanced)
                    (if (not (d-DirectiveTextEdit-chord tag))
                        (set! params 'delete)))
            (else
                    (set! text (d-DirectiveGet-chord-display tag)))))
        (if (eq? params 'delete)
             (begin
                (d-DirectiveDelete-chord tag)
                (d-InfoDialog (_ "Text Span Start deleted. The end text span later should also be deleted ...")))
            (if direction
                (begin 
                    (set! direction (cond ((equal? direction "^") "\\textSpannerUp")((equal? direction "_") "\\textSpannerDown")(else "")))
                    (set! text (d-GetUserInput (_ "Text Spanner") (_ "Give text ") text))
                    (if text
                            (begin
                                (d-DirectivePut-chord-prefix tag  (string-append
                                            (string-append direction "\\override TextSpanner.bound-details.left.text = \\markup {" text "}  ")))

                                        (d-DirectivePut-chord-postfix tag  "\\startTextSpan")
                                        (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
                                        (d-DirectivePut-chord-display tag  text))))))
        (d-SetSaved #f)))
        (set! TextSpanStart::params #f)
