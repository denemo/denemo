;SetMarkedMidiFromEnd
(let ((time #f)(duration (d-GetMidiRecordingDuration)))
	(set! time (d-GetUserInput (_ "Repositioning Marked MIDI Note") (_ "Give interval back from end (seconds):") "30"))
	
	(if (and time (string->number time))
		(begin
			(set! time (string->number time))
			(if (> duration time)
				(d-RepositionRecordedMidi time)
				(d-WarningDialog (string-append (_ "Total Recording Duration is only ") (round (number->string duration)) (_ " Seconds")))))
		(d-WarningDialog (_ "Cancelled"))))
