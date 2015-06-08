;;;;;;;;;;;;;;MvntComment
(let ((tag "MvmntComment"))
(d-DirectivePut-movementcontrol-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_EDITOR))
(d-DirectivePut-movementcontrol-display tag (_ "Click to edit comment"))
(d-DisplayDirectiveTextEditor "movementcontrol" tag))