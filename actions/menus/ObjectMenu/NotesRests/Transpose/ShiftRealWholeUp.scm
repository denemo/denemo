(SingleAndSelectionSwitcher (lambda () 
    (ShiftProto ANS::CalculateWholeRealStepUp)
    (if (d-Directive-chord? "ChordName")        
        (d-DirectivePut-chord-display "ChordName" (DenemoGetNoteAndAccidental)))))


