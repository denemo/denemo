;RecordMidiIn
(let ((pos (GetPosition)) (staff-added 0)(num (max 8 (d-GetMeasuresInStaff))))
	(d-MidiRecord)
	(if (d-RecordingMidi)
		(begin
			(while (d-MoveToStaffUp))
			(if (not (d-Directive-clef? DenemoClickTrack))
					(begin
						(if (not (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
							(begin
								(set! staff-added 1)
								(d-StaffSetSpaceAbove 0)
								(d-CreateClickStaffForMidi num)))
						 (d-DirectivePut-clef-graphic DenemoClickTrack "")
						 (d-DirectivePut-clef-override  DenemoClickTrack DENEMO_OVERRIDE_GRAPHIC)
						 
						 (d-DirectivePut-keysig-graphic DenemoClickTrack "")
						 (d-DirectivePut-keysig-override  DenemoClickTrack DENEMO_OVERRIDE_GRAPHIC)
						 
						(d-MuteStaff)))
			(d-StaffSetSpaceAbove 50)
			(d-GoToPosition #f (+ staff-added (list-ref pos 1)) (list-ref pos 2) (list-ref pos 3)))
		(if (d-GetMidiRecordingDuration)
			(d-AddMeasure))))
