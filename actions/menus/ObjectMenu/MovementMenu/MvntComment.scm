;;;;;;;;;;;;;;MvntComment
(let ((tag "MvmntComment"))
	(if (not (d-Directive-movementcontrol? tag))
		(begin
			(d-DirectivePut-movementcontrol-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_EDITOR))
			(d-DirectivePut-movementcontrol-display tag (_ "Click to edit comment"))))
	(d-DisplayDirectiveTextEditor "movementcontrol" tag))