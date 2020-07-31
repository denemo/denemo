\version "2.20.0"


%%%%%%%% function for offsetting control-points of a TieColumn %%%%%%%%%%%%%%%%%
shapeTieColumn =
#(define-music-function (all-offsets) (list?)
  #{
    \once \override TieColumn #'after-line-breaking =
      #(lambda (grob)
        (let ((ties (ly:grob-array->list (ly:grob-object grob 'ties))))
          (for-each
            (lambda (tie offsets-for-broken-pair)
              (let* ((orig (ly:grob-original tie))
                     (siblings (ly:spanner-broken-into orig)))
                (for-each
                  (lambda (piece offsets-for-piece)
                    (if (pair? offsets-for-piece)
                        (set! (ly:grob-property piece 'control-points)
                              (map
                                (lambda (x y) (coord-translate x y))
                                (ly:tie::calc-control-points piece)
                                offsets-for-piece))))
                  (if (null? siblings)
                      (list orig)
                      siblings)
                  offsets-for-broken-pair)))
            ties all-offsets)))
  #})
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


