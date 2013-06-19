(if (d-MarkStatus)
	(let ()
		(define basedurationlist (duration::SplitTicksToBaseDurations (duration::GetSelectionDurationInTicks)))
		(define newchord (concatenate (MapToSelection ANS::GetChordNotes Note?)))
		(d-DeleteSelectionLeaveEmpty)
		(duration::InsertBaseDurationList basedurationlist  (delete-duplicates newchord))))	

