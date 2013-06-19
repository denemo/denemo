;ChooseTimeSignature
(Doublestroke #f
(cons "4/4" (lambda () (d-InsertTimeSig "4/4"))) ; 1
(cons "2/4" (lambda () (d-InsertTimeSig "2/4")))  ; 2
(cons "3/4" (lambda () (d-InsertTimeSig "3/4"))) ; 3 
(cons "12/8" (lambda () (d-InsertTimeSig "12/8"))) ; 4
(cons "5/4" (lambda () (d-InsertTimeSig "5/4"))) ; 5
(cons "6/8" (lambda () (d-InsertTimeSig "6/8"))) ; 6
(cons "7/8" (lambda () (d-InsertTimeSig "7/8"))) ; 7
(cons "8/4" (lambda () (d-InsertTimeSig "8/4"))) ; 8
(cons "9/8" (lambda () (d-InsertTimeSig "9/8"))) ; 9
(cons "4/2" (lambda () (d-InsertTimeSig "4/2"))) ; 0
)
(d-SetSaved #f)