;DeleteMidiAndClickTrack
(let ((pos (GetPosition)))
	(if (d-RecordingMidi)
		(d-WarningDialog (_ "Stop Recording First"))
		(begin
			(if (d-GetMidiRecordingDuration)
				(d-DeleteRecordedMidi))
			(while (d-MoveToStaffUp))
			(if (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name"))
					(d-DeleteStaff))
			(d-GoToPosition #f (- (list-ref pos 1) 1) (list-ref pos 2) (list-ref pos 3)))))