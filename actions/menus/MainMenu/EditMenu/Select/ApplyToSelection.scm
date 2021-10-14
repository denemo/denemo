;;;ApplyToSelection
(let ((selection (d-GetSelection)))
(if selection
	(begin
		(d-PushPosition)
		(if (not DenemoKeypressActivatedCommand)
			(d-WarningDialog "Close this dialog, then press a shortcut key/shortcut keys to apply the command to every object in the selection\n\nThis command will execute the command you give once with the cursor on  each object in the selection. E.g. if you select 8 notes and give a shortcut for ZoomIn it will zoom 8 times."))
;;;after dialog (if any) dismissed:		
		(let (( var (d-GetCommandFromUser)))
			(MasterMute #t)
			(d-TakeSnapshot)
			(d-IncreaseGuard)
			(if var (ApplyToTaggedSelection  (eval-string (string-append "d-" var))))
			(d-DecreaseGuard)
			(apply d-GoToPosition (car selection))
			(d-SetMark)
			(apply d-GoToPosition (cdr selection))
			(d-SetPoint)
			(d-PopPosition)
			(MasterMute #f)))
	(d-WarningDialog (_ "No selection"))))