;;;Ficta
(let ((tag "Ficta"))
(if (d-Directive-chord? tag)
        (d-DirectiveDelete-chord tag)
        (begin
        (d-DirectivePut-chord-prefix tag "\\once \\set suggestAccidentals = ##t")
        (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
        (d-DirectivePut-chord-minpixels  "Ficta" 20)
        (d-DirectivePut-chord-display tag  "Ficta")))
(d-RefreshDisplay)
(d-SetSaved #f))