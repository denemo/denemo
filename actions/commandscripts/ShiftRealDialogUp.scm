(let ()
(define interval (AskForInterval))
(SingleAndSelectionSwitcher 
	(lambda ()
		(if (Note?) 
			(ANS::ChangeChordNotes (map (lambda (x) (ANS::IntervalCalcUp x interval)) (ANS::GetChordNotes)))
			#f)))) 