;;;TimesigWithBeatStructure
(let ((tag "BeatStructure") (beat #f) (divisions #f))
	(d-InsertTimeSig)
	(d-MoveCursorLeft)
	(set! beat (d-GetUserInput (_ "Beat Structure") (_ "Give beat:\n(smallest note that beams can split at\ne.g. 1/8 for eighth notes.)") "1/16"))
	(if beat
		(begin
			(set! divisions (d-GetUserInput (_ "Beat Structure")
																			(_ "Give grouping of beats desired:\nA set of numbers with spaces between
each number is how many beats before the beam breaks.")
																			 
																			"1 1 1 1"))
			(if divisions
				(begin
					(d-RefreshDisplay)
					(d-SetSaved #f)
					(d-DirectivePut-timesig-prefix tag (string-append "\\overrideTimeSignatureSettings " (d-GetPrevailingTimesig) " " beat " #'(" divisions ") #'()"))
					(d-DirectivePut-timesig-gy tag -10)
					(d-DirectivePut-timesig-graphic tag (string-append "\n" divisions "\nDenemo\n12")))))))