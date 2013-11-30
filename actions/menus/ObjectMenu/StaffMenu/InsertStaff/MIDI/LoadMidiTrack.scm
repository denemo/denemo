;;;;LoadMidiTrack
(let ((n "1")(max (d-GetImportedMidiTracks)))
  (if max
     (begin
		(if LoadMidiTrack::params
			(set! n LoadMidiTrack::params)
			(begin
				(if (= max 1)
					(set! n "1")
					(begin
						(set! n (d-GetUserInput (_ "MIDI Track Selection") 
						(string-append (_ "Select track to import") " 1 ... " (number->string max) ":")
										"1"))))))
			(if n
				(begin
				(set! n (string->number n))
				(if (and (<= n max) (> n 0))
				   (begin
					(while (d-MoveToStaffUp))
					(d-NewStructuredStaff)
					(d-GetImportedMidiTrack n)
					(d-AdvanceMarkedMidi 0))
				  (d-WarningDialog (_ "Out of range"))))
				  (d-InfoDialog (_ "Cancelled"))))
		         (d-WarningDialog (_ "No MIDI file loaded"))))
		
