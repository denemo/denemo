(let ()
(define interval (AskForInterval))
(SingleAndSelectionSwitcher 
	(lambda ()
		(if (Note?) 
			(ANS::ChangeChordNotes (map (lambda (x) (ANS::IntervalCalcDown x interval)) (ANS::GetChordNotes)))
			#f)))) 