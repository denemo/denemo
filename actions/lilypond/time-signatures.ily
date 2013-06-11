#(define ((alternate-time up down upp downp) grob)
   (grob-interpret-markup grob
     #{
       \markup
         \override #'(baseline-skip . 0)
         \number
         \line {
                 \column { #up #down }
                 \column { #upp #downp }
         }
     #}))
