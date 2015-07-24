;;;Cresc
(if (Music?)
    (let ((tag "Cresc"))
    (if (equal? Cresc::params "edit")
        (set! Cresc::params #f));not yet implemented
    (if (d-Directive-chord? tag)
        (if Cresc::params
            (let ((type (car (list-ref Cresc::params 0)))   (params (cdr (list-ref Cresc::params 0))))
                (if (eq? type 'direction)
                    (d-DirectivePut-chord-postfix tag  (string-append params "\\cresc"))
                    (d-WarningDialog (_ "Not yet implemented"))))
            (begin
                (d-DirectiveDelete-chord tag)
                (d-InfoDialog (_ "cresc. start deleted. The end crescendo should also be deleted ..."))))
        (begin
            (d-DirectivePut-chord-postfix tag  "-\\cresc")
            (d-DirectivePut-chord-display tag  "cresc.")))
    (d-SetSaved #f)))
;;;End of scheme script
