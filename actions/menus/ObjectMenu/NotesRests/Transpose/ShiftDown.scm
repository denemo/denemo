(SingleAndSelectionSwitcher (lambda () 
    (ShiftProto ANS::CalculateDiatonicStepDown)
    (if (d-Directive-chord? "ChordName")        
        (d-DirectivePut-chord-display "ChordName" (DenemoGetNoteAndAccidental)))))
