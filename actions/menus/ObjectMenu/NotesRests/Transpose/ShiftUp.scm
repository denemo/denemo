(SingleAndSelectionSwitcher (lambda () 
    (ShiftProto ANS::CalculateDiatonicStepUp)
    (if (d-Directive-chord? "ChordName")        
        (d-DirectivePut-chord-display "ChordName" (DenemoGetNoteAndAccidental)))))
