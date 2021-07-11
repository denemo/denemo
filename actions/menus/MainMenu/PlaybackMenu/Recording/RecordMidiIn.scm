;RecordMidiIn
(let ((pos (GetPosition))(num (d-GetMeasuresInStaff)))
	(d-MidiRecord)
	(if (d-RecordingMidi)
		(begin
			(while (d-MoveToStaffUp))
			(if (not (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
					(begin
						(d-StaffSetSpaceAbove 0)
						(d-CreateClickStaffForMidi num)
						 (d-DirectivePut-clef-graphic "NoDisplay" "")
						 (d-DirectivePut-clef-override  "NoDisplay"  5)
						(d-MuteStaff)))
			(d-StaffSetSpaceAbove 50)
			(d-GoToPosition #f (+ 1 (list-ref pos 1)) (list-ref pos 2) (list-ref pos 3)))
		(d-ExtendClickTrack)))