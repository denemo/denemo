;;AppendPostscript
(let ((tag "AppendPostscript")(filename #f)(scale #f)(space-above #f))
    (if (d-Directive-movementcontrol? tag)
        (let ((data (eval-string (d-DirectiveGet-movementcontrol-data tag))))
            (set! filename  (list-ref data 0))
            (set! scale (list-ref data 1))
            (set! space-above (list-ref data 2))))
    (if (not filename)
        (begin
            (set! filename "...../drawing.eps")
            (set! scale "100")
            (set! space-above "14")))
            
            
    (set! filename (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give full .eps filename:") filename))
    (set! scale (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give width required:")  scale))
    (set! space-above (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space above required:") space-above))
    (if (and filename scale space-above)
        (begin
                    (d-DirectivePut-movementcontrol-postfix tag
                        (string-append "\\markup {\\vspace #" space-above " \\epsfile #X #" scale " #\"" filename "\"}"))
                    (d-DirectivePut-movementcontrol-override tag DENEMO_OVERRIDE_AFFIX)
                     (d-DirectivePut-movementcontrol-data tag (string-append "(list \"" filename "\" \"" scale "\" \"" space-above "\")")))
                (begin
                    (if (equal? (_ "y") (d-GetUserInput  (_ "Encapsulated Postscript File") (_ "Delete appended postscript?") (_ "n")))
                    (begin
                        (d-DirectiveDelete-movementcontrol tag)
                        (d-InfoDialog "Appended Postscript Deleted"))))))
(d-SetSaved #f)
