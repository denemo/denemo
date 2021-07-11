;RecordMidiIn
(let ((pos (GetPosition))(num (d-GetMeasuresInStaff)))
	(d-MidiRecord)
	(if (d-RecordingMidi)
		(begin
			(while (d-MoveToStaffUp))
			(if (not (d-Directive-clef? DenemoClickTrack))
					(begin
						(d-StaffSetSpaceAbove 0)
						(d-CreateClickStaffForMidi num)
						 (d-DirectivePut-clef-graphic DenemoClickTrack "")
						 (d-DirectivePut-clef-override  DenemoClickTrack DENEMO_OVERRIDE_GRAPHIC)
						(d-MuteStaff)))
			(d-StaffSetSpaceAbove 50) (disp "Set 50")
			(d-GoToPosition #f (+ 1 (list-ref pos 1)) (list-ref pos 2) (list-ref pos 3)))
		(d-ExtendClickTrack)))
