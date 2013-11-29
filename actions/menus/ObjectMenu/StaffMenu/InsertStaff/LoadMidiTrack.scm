;;;;LoadMidiTrack
(let ((n "1"))
	(if LoadMidiTrack::params
		(set! n LoadMidiTrack::params)
		(begin
			(set! n (d-GetUserInput (_ "MIDI Track Selection") (_ "Select track to import") "1"))))
	(if n
		(begin
		(set! n (string->number n))
		(while (d-MoveToStaffUp))
		(d-NewStructuredStaff)
		(d-GetImportedMidiTrack n)
		(d-AdvanceMarkedMidi 0))))
