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


#(define ((alternate-time-parens up down upp downp) grob)
   (grob-interpret-markup grob
     #{
       \markup
         \override #'(baseline-skip . 0)

         \number
         \line {
                 \column { #up #down } \lower #1.2 "("
                 \column {  #upp #downp }  \lower #1.2 ")"
         }

     #}))
