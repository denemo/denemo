;;;ExtendTheClickTrack
(let ((time 0)(rec-time (d-GetMidiRecordingDuration)))
	(if rec-time
		(begin
			(d-PushPosition)
			(while (d-MoveToStaffUp))
			(if (d-Directive-clef? DenemoClickTrack)
				(let ((num 0))
					(d-MoveToBeginning)
					(d-DirectiveDelete-standalone "MuteStaff");remove the speaker icon
					(d-EvenOutStaffLengths)
					(set! num (d-GetMeasuresInStaff))
					(if (d-MoveToMeasureRight)
						(d-DeleteFromCursorToEnd 'this))
					(set! time (d-GetTimeAtCursor))
					(while (or (< time rec-time) (< (d-GetMeasure) num))
						(d-FillMeasure #t)
						(d-MoveToMeasureRight)
						(set! time (d-GetTimeAtCursor)))
					
					(DenemoSetPlaybackEnd)
					(d-MuteStaff)))
					(d-PopPosition))
		(d-WarningDialog (_ "No Midi recording"))))