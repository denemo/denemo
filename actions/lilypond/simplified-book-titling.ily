#(define (string-upper-case str) str)
#(define-markup-command (piece-title layout props title) (markup?)
  (interpret-markup layout props
   (markup #:column (#:vspace 1
                     #:fill-line (#:fontsize 4 title)
                     #:vspace 1))))
                     
