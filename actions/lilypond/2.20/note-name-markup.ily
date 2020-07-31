\version "2.20.0"
#(define (denemo-bass-inversion pitch)
(let* ((alt (ly:pitch-alteration pitch)))
  (make-sans-markup (make-scale-markup '(2.50 . 3.0) (make-line-markup
    (list
       (make-raise-markup 1;1.5 
        (make-scale-markup '(0.75 . 0.5) 
            (make-bold-markup (make-simple-markup "/"))))
       (make-hspace-markup -0.3)
       (make-raise-markup 0.5
           (make-bold-markup 
            (make-scale-markup '(0.4 . 0.6) 
                (make-simple-markup 
                    (vector-ref #("C" "D" "E" "F" "G" "A" "B") (ly:pitch-notename
                        pitch))))))
       (make-raise-markup 0.5
          (if (= alt NATURAL)
            (make-hspace-markup 0)
            (make-line-markup
                (list
                  (make-hspace-markup 0.1)
                  (make-fontsize-markup -7 
                  (if (= alt SHARP)
                    (make-raise-markup 0.1
                            (alteration->text-accidental-markup alt))
                    (make-raise-markup 0.2
                            (alteration->text-accidental-markup alt))))))))))))))


