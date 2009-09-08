;; Get the Duration of current Note/Chord/Rest and give out either the basenote's duration or the number of dots
(define (d-GetNoteDurationBase input what)
 (let ((basenote 0)
 (numdots 0)
 (list #t))
 (set! list (string-split input #\.))
 (set! basenote (car list))
 (set! basenote (string->number basenote))
 (set! numdots (length (cdr list)))
  (if (= what 1)
  numdots
  basenote)
))

;;Used to diminish or augment the current Note/Chord/Rest by factor 2. One Argument to switch the way the scripts operates: * or / .   By Nils Gey
(define (d-ChangeDurationByFactorTwo math) 

(define noteDuration (d-GetNoteDurationBase (d-GetNoteDuration) 0))
(define numberOfDots (d-GetNoteDurationBase (d-GetNoteDuration) 1))

(define (ChangeTheNote)
 ; 1st number means string position. 2nd means factor of diminuation.
 (case (math noteDuration 2)
   ((1)		(d-Change0))
   ((2)		(d-Change1))
   ((4)		(d-Change2))
   ((8)		(d-Change3))
   ((16)	(d-Change4))
   ((32)	(d-Change5))
   ((64)	(d-Change6))
   (else   #f )
))

(define (AddMoreDots x x-max dx) ; initial value - max steps - step value
   (if (<= x x-max)
      (begin
      	(d-AddDot)
        (AddMoreDots (+ x dx) x-max dx))))

(if (ChangeTheNote)
 (AddMoreDots 1 numberOfDots 1) #f)
 
 )
