;;;SplitMovementAtCursor
(if (d-GetSaved)
    (let ((timesig (d-GetPrevailingTimesigAsLilyPond))
            (keysig (d-GetPrevailingKeysigName))) 
        (while (d-MoveToStaffUp))
        (d-SetMark)
        (d-GoToEnd) ;;;extends selection
        (while (d-StaffDown)) ;;;extends selection
        (d-Cut)
        (d-InsertMovementAfter)
        (while (d-MoveToStaffUp))
        (d-InitialTimeSig timesig)
         (d-InitialKey keysig)
          (d-Paste))
     (d-WarningDialog (_ "Score is not saved")))