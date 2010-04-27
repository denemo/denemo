(define (ShiftUp)
(define newList #f)

(if (d-GetNotes)
(begin
(set! newList (string-tokenize(d-GetNotes)) )

 (let transformChordList ((i 0) )
   (if (<= i (-(length newList )1)) 
      (begin
        (set! newList (replace-nth newList i (ANS-7::CalculateDiatonicStep (list-ref newList i)  ))) ; #t as optional argument for Calculate... means step down instead of up.
        (transformChordList (+ i 1)))))

   (d-ChangeChordNotes (string-join newList))
 )
#f ; if its not a note/chord
))

(define (ShiftDown)
(define newList #f)

(if (d-GetNotes)
(begin
(set! newList (string-tokenize(d-GetNotes)) )

 (let transformChordList ((i 0) )
   (if (<= i (-(length newList )1)) 
      (begin
        (set! newList (replace-nth newList i (ANS-7::CalculateDiatonicStep (list-ref newList i) #t ))) ; #t as optional argument for Calculate... means step down instead of up.
        (transformChordList (+ i 1)))))

   (d-ChangeChordNotes (string-join newList))
 )
#f ; if its not a note/chord
))
