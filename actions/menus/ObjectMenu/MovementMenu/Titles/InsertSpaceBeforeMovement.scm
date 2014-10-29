;;InsertSpaceBeforeMovement
(let ((tag "InsertSpaceBeforeMovement")(params InsertSpaceBeforeMovement::params)(space #f))
	(set! space (d-DirectiveGet-movementcontrol-data tag))
	(if params
		(set! space params))
	(if (not space)
		(set! space "0.5"))
        (set! space (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space between title and score:") space))
        (if (and space (string->number space))
        	(begin
		(d-DirectivePut-movementcontrol-prefix tag
		                (string-append "\\markup {\\vspace #" space " }"))
		(d-DirectivePut-movementcontrol-data tag  space)             
		(d-SetSaved #f))
                (let ((confirm (d-GetUserInput (d-DirectiveGet-standalone-display tag) (_ "Delete spacer?") (_ "y"))))
                 (if (equal? confirm (_ "y"))
                    (begin
                        (d-DirectiveDelete-movementcontrol tag)
                        (d-SetSaved #f))
                     (d-InfoDialog (_ "Cancelled"))))))
