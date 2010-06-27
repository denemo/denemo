(load "/home/nils/git-lalc/lalr-scm/lalr.scm")

(define mxml2lily2denemo

  (lalr-parser
   ; Terminal symbols
   (ID + - * /)
   ; Productions
   (e (e + t)    : (+ $1 $3)
      (e - t)    : (- $1 $3)
      (t)        : $1)
   (t (t * f)    : (* $1 $3)
      (t / f)    : (/ $1 $3)
      (f)        : $1)
   (f (ID)       : $1)))



