(SingleAndSelectionSwitcher (lambda () 
    (ShiftProto ANS::CalculateHalfRealStepUp)
    (if (d-Directive-chord? "ChordName")        
        (d-DirectivePut-chord-display "ChordName" (DenemoGetNoteAndAccidental)))))


