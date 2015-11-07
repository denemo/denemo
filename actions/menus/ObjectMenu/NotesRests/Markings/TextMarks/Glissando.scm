;;;Glissando
(let ((tag "Glissando")(params Glissando::params)(length #f)) 
    (if (equal? params "edit")
        (let ((choice (RadioBoxMenu (cons (_ "Minimum Length") 'length)
                        (cons (_ "Delete") 'delete)
                        (cons (_ "Advanced") 'advanced))))
          (case choice
                ((length)
                    (set! params #f)
                    (set! length (d-GetUserInput (_ "Glissando") (_ "Give minimum length for glissando line: ") "2")))
                ((delete)
                    (d-DirectiveDelete-chord tag))
                ((advanced)
                    (d-DirectiveTextEdit-chord tag)))))
     (if (not params)
        (begin
        (if (and length (string->number length))
        
            (begin
                (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
                (d-DirectivePut-chord-prefix tag (string-append 
                "\\once \\override Glissando #'minimum-length = #" length 
                " \\once \\override Glissando #'springs-and-rods = #ly:spanner::set-spacing-rods "))))
        (d-DirectivePut-chord-display tag  "Gliss" )
        (d-DirectivePut-chord-postfix tag  "\\glissando")
        (d-DirectivePut-chord-minpixels tag 20)))

    (d-SetSaved #f)
    (d-RefreshDisplay))
