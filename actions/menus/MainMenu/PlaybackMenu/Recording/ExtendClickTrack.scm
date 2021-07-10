;;;ExtendTheClickTrack
(let ((time 0)(rec-time (d-GetMidiRecordingDuration)))
	(d-PushPosition)
	(while (d-MoveToStaffUp))
	(if (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name"))
		(begin
			(d-MoveToBeginning)
			(d-DirectiveDelete-standalone "MuteStaff");remove the speaker icon
			(d-MoveToEnd)
			(set! time (d-GetTimeAtCursor))
			(while (< time rec-time)
				(d-FillMeasure)
				(d-MoveToEnd)
				(set! time (d-GetTimeAtCursor)))
			(DenemoSetPlaybackEnd)
			(d-MuteStaff)
			(d-PopPosition))))
