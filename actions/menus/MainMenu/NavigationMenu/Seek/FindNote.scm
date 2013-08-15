;;;;;; FindNote
    
(define-once FindNote::sought "des")
    
(let loop ((sought #f))
  (define (note-equal? needle haystack)
  (= (string-prefix-length needle haystack) (string-length needle)))
;(format #t "in the loop with ~a ~a\n\n" sought (d-GetNote))
  (if sought 
  (if (and (d-NextNote) (not (note-equal? sought (d-GetNote))))
      (loop sought))
  (begin 
   (set! sought (d-GetUserInput "Find Note" "Give the note to find" FindNote::sought))
   (if sought (begin (set! FindNote::sought sought) (loop sought))))))
(d-RefreshDisplay)
;;;;;;;;;;;;;;;;;

