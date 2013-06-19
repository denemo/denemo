
(let ((N 4)(Extra? #t)) ;set N to be number of bars to preview
(define (MoveRightNBars NumBars)
	(if (> NumBars 0)
		(begin 	
			(d-MoveToMeasureRight)
			(MoveRightNBars (- NumBars 1)))
))
(d-PushPosition)
(while (d-MoveToStaffUp))	
(while (d-PrevObjectInMeasure))	;make sure we get to start o' bar.
(d-TypesettingOn)
 (d-MoveCursorLeft)	;get on the typesetting directive
(d-PushPosition)
(MoveRightNBars N)
(d-TypesettingOff)
(d-MoveCursorLeft)
(d-PushPosition)
(d-MoveToBeginning)
(if (d-DirectiveGet-standalone-postfix "TypesetOn") (set! Extra? #f)) ;if going to the beginning, we ended up on top of the TypesettingOn, we don't do this TypeOff:
(if Extra? (begin (d-TypesettingOff) (d-MoveCursorLeft)))
(d-PrintPreview)
(d-DirectiveDelete-standalone "TypesetOff")	
(d-PopPosition)
(d-DirectiveDelete-standalone "TypesetOff")	
(d-PopPosition)
(d-DirectiveDelete-standalone "TypesetOn")	
(d-PopPosition)
) ;let