;;;RaggedRight
(let ((tag "RaggedRight"))
(if (d-Directive-movementcontrol? tag)
       (d-DirectiveDelete-movementcontrol tag)
       (begin
               (d-DirectivePut-movementcontrol-postfix tag "\\layout {ragged-right=##f}\n")
               (d-DirectivePut-movementcontrol-display tag (_ "Flush Right"))
               (d-DirectivePut-movementcontrol-override tag 
                    DENEMO_OVERRIDE_AFFIX)))
(d-SetSaved #f))