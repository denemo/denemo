;;;HideMovement
(let ((tag "HideMovement"))
;delete obsolete directives
(d-DirectiveDelete-layout "HiddenMovement")
(d-DirectiveDelete-header "HiddenMovement")
(if HideMovement::params
	    (d-DirectiveDelete-movementcontrol tag))
(if (d-Directive-movementcontrol? tag)
    (begin
        (d-DirectiveDelete-movementcontrol tag)
        (d-InfoDialog (_ "Movement will be typeset normally")))
    (begin
        (d-DirectivePut-movementcontrol-prefix tag "\\void ")
        (d-DirectivePut-movementcontrol-display tag (_ "Non-printing Movement"))
        (if (not HideMovement::params)
       		 (d-InfoDialog (_ "This movement will not be typeset. You must have at least one movement that is typeset"))))))
(d-SetSaved #f)