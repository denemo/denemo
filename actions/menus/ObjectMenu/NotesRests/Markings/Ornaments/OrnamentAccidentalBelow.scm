;;;AccidentalBelow
(let ((tag "AccidentalBelow"))
    (d-SetSaved #f)
    (if (d-Directive-chord? tag)
        (d-DirectiveDelete-chord tag)
        (begin
        (d-DirectivePut-chord-prefix tag 
              "\\once \\override TextScript.script-priority = #-100 ")
        (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX))))