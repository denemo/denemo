(define (ShiftProto method)
 ; Get all notes on cursor position and create a list with new values which then exchanges the current notes on cursor position

(if (d-GetNotes)
    (ANS-7::ChangeChordNotes  
  	(map method (ANS-7::GetChordNotes))
  	)
    #f ; not a note/chord
  )
)

(define (ShiftUp)
 (ShiftProto ANS-7::CalculateDiatonicStepUp)
)

(define (ShiftDown)
 (ShiftProto ANS-7::CalculateDiatonicStepDown)
)

(define (ShiftRealOctaveUp) ;in reality this is not shift but transpose. But there are too many functions with the name transpose already...
 (ShiftProto ANS-7::CalculateRealOctaveUp)
)

(define (ShiftRealOctaveDown) ;in reality this is not shift but transpose. But there are too many functions with the name transpose already...
 (ShiftProto ANS-7::CalculateRealOctaveDown)
)
