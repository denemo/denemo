(define (ShiftProto method)
 ; Get all notes on cursor position and create a list with new values which then exchanges the current notes on cursor position

(if (Note?) 
    (ANS::ChangeChordNotes  
  	(map method (ANS::GetChordNotes))
  	)
    #f ; not a note/chord
  )
)

(define (ShiftUp)
 (ShiftProto ANS::CalculateDiatonicStepUp)
)

(define (ShiftDown)
 (ShiftProto ANS::CalculateDiatonicStepDown)
)

(define (ShiftRealOctaveUp) ;in reality this is not shift but transpose. But there are too many functions with the name transpose already...
 (ShiftProto ANS::CalculateRealOctaveUp)
)

(define (ShiftRealOctaveDown) ;in reality this is not shift but transpose. But there are too many functions with the name transpose already...
 (ShiftProto ANS::CalculateRealOctaveDown)
)
