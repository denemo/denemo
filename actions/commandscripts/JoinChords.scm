;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(if (d-MarkStatus)
	(let ()
		(define basedurationlist (duration::SplitTicksToBaseDurations (duration::GetSelectionDurationInTicks)))
		(d-DeleteSelectionLeaveEmpty)
		(duration::InsertBaseDurationList basedurationlist (ANS::GetDiatonic (ANS::Ly2Ans (string->symbol (GetCursorNoteAsLilypond)))))))