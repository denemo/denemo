;;StartTuplet
( let ((top #f) (bottom #f) (DirectiveText #f) (DirectiveDisplay #f) )
(set! top (d-GetUserInput "Enter tuplet numerator " "Enter the fraction to multiply the duration by,\nnumerator first. E.g., for triplets, enter 2, then 3. \nNumerator:" "2" ) )
(set! bottom (d-GetUserInput "Enter tuplet denominator" "Enter the fraction's denominator:" "3" ) )
(d-StartTriplet)
(if (boolean? (d-GetTuplet))
	(d-MoveCursorLeft))
(d-SetTuplet  (string-append top "/" bottom ) )	
(d-RefreshDisplay))
