(SingleAndSelectionSwitcher (lambda () 
    (ShiftProto ANS::CalculateHalfRealStepDown)
    (if (d-Directive-chord? "ChordName")        
        (d-DirectivePut-chord-display "ChordName" (DenemoGetNoteAndAccidental)))))


