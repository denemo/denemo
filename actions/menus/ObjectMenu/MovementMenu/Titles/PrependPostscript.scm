;;PrependPostscript
(let ((tag "PrependPostscript")(filename #f)(scale #f)(space-below #f))
    (if (d-Directive-movementcontrol? tag)
        (let ((data (eval-string (d-DirectiveGet-movementcontrol-data tag))))
            (set! filename  (list-ref data 0))
            (set! scale (list-ref data 1))
            (set! space-below (list-ref data 2))))
    (if (not filename)
        (begin
            (set! filename "...../drawing.eps")
            (set! scale "100")
            (set! space-below "14")))
            
            
    (set! filename (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give full .eps filename:") filename))
    (set! scale (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give width required:")  scale))
    (set! space-below (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space below required:") space-below))
    (if (and filename scale space-below)
        (begin
                    (d-DirectivePut-movementcontrol-prefix tag
                        (string-append "\\markup {\\epsfile #X #" scale " #\"" filename "\" \\vspace #" space-below " }"))
                     (d-DirectivePut-movementcontrol-data tag (string-append "(list \"" filename "\" \"" scale "\" \"" space-below "\")")))
                (begin
                    (if (equal? (_ "y") (d-GetUserInput  (_ "Encapsulated Postscript File") (_ "Delete prepended postscript?") (_ "n")))
                    (begin
                        (d-DirectiveDelete-movementcontrol tag)
                        (d-InfoDialog (_ "Prepended Postscript Deleted")))))))
(d-SetSaved #f)
