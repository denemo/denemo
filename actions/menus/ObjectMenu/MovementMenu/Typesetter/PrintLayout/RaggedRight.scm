;;;RaggedRight
(let ((tag "RaggedRight"))
(if (d-Directive-layout? tag)
       (d-DirectiveDelete-layout tag)
       (begin
               (d-DirectivePut-layout-postfix tag "ragged-right=##f\n")
               (d-DirectivePut-layout-display tag (_ "Flush Right"))
               (d-DirectivePut-layout-override tag 
                    DENEMO_OVERRIDE_AFFIX)))
(d-SetSaved #f))