;;;ShiftRealDialogDown
(let ((interval (AskForInterval)))
(SingleAndSelectionSwitcher 
    (lambda ()
        (if (Note?) 
            (begin
                (ANS::ChangeChordNotes (map (lambda (x) (ANS::IntervalCalcDown x interval)) (ANS::GetChordNotes)))
                (if (d-Directive-chord? "ChordName")        
                    (d-DirectivePut-chord-display "ChordName" (DenemoGetNoteAndAccidental))))
            #f)))) 
