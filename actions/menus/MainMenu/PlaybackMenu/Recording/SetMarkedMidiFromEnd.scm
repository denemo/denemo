;SetMarkedMidiFromEnd
(let ((time #f)(duration (d-GetMidiRecordingDuration)))
	(set! time (d-GetUserInput (_ "Repositioning Marked MIDI Note") (_ "Give interval back from end (seconds):") (number->string (/  (round (* 10  duration)) 10))))
	
	(if (and time (string->number time))
		(begin
			(set! time (string->number time))
			(if (< duration time)
				(set! time duration))
			(d-RepositionRecordedMidi time))
		(d-WarningDialog (_ "Cancelled"))))
