;;;SetStaffRangeFromChord
(if (Chord?)
    (d-SetStaffRange)
    (d-WarningDialog (_ "The cursor must be on a chord with the highest note and lowest note set to the range desired.")))
