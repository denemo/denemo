;;;ApplyToSelection
(if (not DenemoKeypressActivatedCommand)
	(d-WarningDialog "Close this dialog, then press a shortcut key/shortcut keys to apply the command to every object in the selection\n\nThis command will execute the command you give once with the cursor on  each object in the selection. E.g. if you select 8 notes and give a shortcut for ZoomIn it will zoom 8 times."))
	
(let (( var (d-GetCommandFromUser)))
	(MasterMute #t)
	(d-TakeSnapshot)
	(d-IncreaseGuard)
	(if var (ApplyToTaggedSelection  (eval-string (string-append "d-" var))))
	(d-DecreaseGuard)
	(MasterMute #f))
