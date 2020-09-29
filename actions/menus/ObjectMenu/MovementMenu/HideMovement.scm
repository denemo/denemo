;;;HideMovement
(let ()
;delete obsolete directives
(d-DirectiveDelete-layout "HiddenMovement")
(d-DirectiveDelete-header "HiddenMovement")
(if HideMovement::params
        (if (not (d-ToggleSketch))
        	(d-ToggleSketch))
        (begin
		(if (d-ToggleSketch)
 			 (d-InfoDialog (_ "This movement is a sketch - it will not be typeset with the default layout. You must have at least one movement that is typeset"))
        		(d-InfoDialog (_ "Movement will be typeset normally")))))
;;; Reset the movement indicator so it shows red
(if (= 1 (d-GetMovement))
    (begin
        (d-NextMovement)
        (d-PreviousMovement))
    (begin
        (d-PreviousMovement)
        (d-NextMovement)))
(d-SetSaved #f))
