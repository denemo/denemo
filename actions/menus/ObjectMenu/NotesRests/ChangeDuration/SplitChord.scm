 
(if SplitChord::params
  (SplitChord SplitChord::params)
(Doublestroke SplitChord
(cons "Split into 1 :)" (lambda () (SplitChord 1)))  ;1
(cons "Split into 2" (lambda () (SplitChord 2)))  ;2
(cons "Split into 3" (lambda () (SplitChord 3)))  ;3
(cons "Split into 4" (lambda () (SplitChord 4)))  ;4
(cons "Split into 5" (lambda () (SplitChord 5)))  ;5
(cons "Split into 6" (lambda () (SplitChord 6)))  ;6
(cons "Split into 7" (lambda () (SplitChord 7)))  ;7
(cons "Split into 8" (lambda () (SplitChord 8)))  ;8
(cons "Split into 9" (lambda () (SplitChord 9)))  ;9
))
