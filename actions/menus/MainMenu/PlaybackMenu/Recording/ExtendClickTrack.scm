;;;ExtendTheClickTrack
(let ((time 0)(rec-time (d-GetMidiRecordingDuration)))
	(if rec-time
		(begin
			(d-PushPosition)
			(while (d-MoveToStaffUp))
			(if (d-Directive-clef? DenemoClickTrack)
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
					(d-MuteStaff)))
					(d-PopPosition))))
