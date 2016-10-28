;;; OpenParenthesizeChord
(let ((tag "OpenParenthesizeChord"))
 (if (d-Directive-chord? tag)
    (d-DirectiveDelete-chord  tag)
    (begin
        (d-LilyPondDefinition (cons "startParenthesize" 
 "\\once \\override ParenthesesItem.stencils = #(lambda (grob)
        (let ((par-list (parentheses-item::calc-parenthesis-stencils grob)))
          (list (car par-list) point-stencil )))"))
        (d-DirectivePut-chord-prefix tag "\\startParenthesize\\parenthesize ")
        (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
        (d-DirectivePut-chord-display tag "(")))
(d-SetSaved #f))
