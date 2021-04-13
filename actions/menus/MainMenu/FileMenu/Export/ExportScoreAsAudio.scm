;ExportScoreAsAudio
(let ((num-movements (d-GetMovementsInScore)))
	(define callback "(d-ToggleRecordingAudio)(d-ExportRecordedAudio)")
	(define inter-movement-pause "2")
	(if (> num-movements 1)
		(set! inter-movement-pause 
			(d-GetUserInput (_ "Recording Audio") (_ "Give pause between movements (seconds):") "2")))
	(if (and (string? inter-movement-pause) (string->number inter-movement-pause))
		(begin
			(while (> num-movements 1)
				(set! callback (string-append "(NextNonSketchMovement)(sleep " inter-movement-pause ")" "(d-Performance \"" (scheme-escape callback) "\")"))
				(set! num-movements (1- num-movements)))
			(d-GoToPosition 1 1 1 1)
			(d-ToggleRecordingAudio)
			(d-Performance callback))
		(d-WarningDialog (_ "Cancelled"))))