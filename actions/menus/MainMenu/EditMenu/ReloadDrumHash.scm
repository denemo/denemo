;;;;Prepares a Hash Table from a .denemo file where a user can put his own drum notation. 
;;;;Format: Each drum instrument has its own staff. Two notes in each: First note/value=GM-note, second user-note
;;;;by Nils Gey, 01/2010

(d-NewWindow)
(if (d-OpenMyTemplate) 
(begin
;;Go to first Staff
(let gotoTopStaff ()
	(if  (d-MoveToStaffUp)
	(gotoTopStaff)))

(define DrumHash (make-hash-table 50))

(let createDrumHashTable ()
	(d-MoveToBeginning)
	(hashq-set! DrumHash (string->symbol (GetLowestNote)) (begin (d-NextNote) (GetLowestNote) ))
	(if (and (d-MoveToStaffDown) (GetLowestNote) )
		(createDrumHashTable))
		)
))

(d-Close)