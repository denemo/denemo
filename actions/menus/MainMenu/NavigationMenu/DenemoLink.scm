;;;DenemoLink
(let ((tag "DenemoLink"))
    (if (d-Directive-standalone? tag)
        (let ((link (d-DirectiveGet-standalone-data tag)))
            (if (not link)
                 (begin
                    (set! link (d-DirectiveGet-standalone-postfix tag))
                    (if link
                        (set! link (string-trim-both link   (lambda (c)(or (eqv? c #\{) (eqv? c #\%))))))))
            (if link
                (begin
                    (d-OpenSource link)
                    (d-MoveCursorRight))
                (d-WarningDialog (_ "There is no link here, open the source document and click on it to place one."))))))
