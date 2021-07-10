;RecordMidiIn
(let ()
	(d-MidiRecord)
	(if (d-RecordingMidi)
		(begin
			(d-PushPosition)
			(while (d-MoveToStaffUp))
			(if (not (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
					(begin
						(d-CreateClickStaffForMidi 1)
						(d-MuteStaff)))
			(d-PopPosition))
		(d-ExtendClickTrack)))