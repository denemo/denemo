(define (Navigation::CursorUpN n) 
  (begin (if (> n 0) (begin (d-CursorUp) (Navigation::CursorUpN (- n 1))))
    (if (< n 0) (begin (d-CursorDown) (Navigation::CursorUpN (+ n 1))))))