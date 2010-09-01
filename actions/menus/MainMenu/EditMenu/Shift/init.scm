(define (ShiftUp)
  (if (d-GetNotes)
    (ANS-7::ChangeChordNotes 
  	(map ANS-7::CalculateDiatonicStepUp (ANS-7::GetChordNotes)) ; Get all notes on cursor position and create a list with shifted-up values
  	)
    #f ; not a note/chord
  )
)

(define (ShiftDown)
(if (d-GetNotes)
    (ANS-7::ChangeChordNotes 
  	(map ANS-7::CalculateDiatonicStepDown (ANS-7::GetChordNotes)) ; Get all notes on cursor position and create a list with shifted-up values
  	)
    #f ; not a note/chord
  )
)
