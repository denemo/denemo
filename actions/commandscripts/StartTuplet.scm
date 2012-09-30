;;StartTuplet
( let ((top #f) (bottom #f) (DirectiveText #f) (DirectiveDisplay #f) )
(set! top (d-GetUserInput (_ "Enter tuplet numerator ") (_ "Enter the fraction to multiply the duration by,\nnumerator first. E.g., for triplets, enter 2, then 3. \nNumerator:") "2" ) )
(set! bottom (d-GetUserInput (_ "Enter tuplet denominator") (_ "Enter the fraction's denominator:") "3" ) )
(d-StartTriplet)
(if (boolean? (d-GetTuplet))
	(d-MoveCursorLeft))
(d-SetTuplet  (string-append top "/" bottom ) )	
(d-RefreshDisplay))
