;;;Dim
(if (Music?)
    (let ((tag "Dim"))
    (if (equal? Dim::params "edit")
        (set! Dim::params #f));not yet implemented
    (if (d-Directive-chord? tag)
        (if Dim::params
            (let ((type (car (list-ref Dim::params 0)))   (params (cdr (list-ref Dim::params 0))))
                (if (eq? type 'direction)
                    (d-DirectivePut-chord-postfix tag  (string-append params "\\dim"))
                    (d-WarningDialog (_ "Not yet implemented"))))
            (begin
                (d-DirectiveDelete-chord tag)
                (d-InfoDialog (_ "dim. start deleted. The end diminuendo should also be deleted ..."))))
        (begin
            (d-DirectivePut-chord-postfix tag  "-\\dim")
            (d-DirectivePut-chord-display tag  "dim.")))
    (d-SetSaved #f)))
