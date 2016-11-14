(SingleAndSelectionSwitcher (lambda () 
    (ShiftProto ANS::CalculateWholeRealStepDown)
    (if (d-Directive-chord? "ChordName")        
        (d-DirectivePut-chord-display "ChordName" (DenemoGetNoteAndAccidental)))))


