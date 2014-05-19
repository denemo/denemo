;;;ArticulationOutsideSlur
(let ((tag "ArticulationOutsideSlur"))
    (d-SetSaved #f)
    (if (d-Directive-chord? tag)
        (d-DirectiveDelete-chord tag)
        (begin
        (d-DirectivePut-chord-prefix tag 
              "\\once \\override Script.avoid-slur = #'outside ")
        (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX))))
