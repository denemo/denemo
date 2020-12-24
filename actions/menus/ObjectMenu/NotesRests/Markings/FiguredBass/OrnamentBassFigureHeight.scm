;OrnamentBassFigureHeight
;avoid ornaments clashing with bass figures
(let ((transpose 0))
(define (OrnamentHeight)
	(if (d-Directive-chord? "ToggleStaccato")
		1 
		(if (d-Directive-chord? "ToggleTrill") 
			(if (d-Directive-score? "Allow\ntrill") 
				1.75
				2.75)
			0)))
(define (AdjustFigures)
    (define (GetNoteStaffPosition)
        (define height (d-GetNoteStaffPosition))
        (if height
                (+ transpose height)
                #f))
    (d-MoveToBeginning)
    (while (d-MoveToStaffDown))
    (while (d-NextNote)
        (let ((fig (d-GetBassFigure)) (ornament-height (OrnamentHeight))(height  (GetNoteStaffPosition)))
			(if (and fig height (positive? ornament-height) (> height 1))
					(d-AdjustBassFigureHeight (cons "FBHeight1" (number->string 
						;(+  (* 1.75 (car thelist)) -1.250 (/ (+ 5 height) 2)))))
					(if (< height 4) 5.25
						(+ ornament-height 4.75 (* (- height 4) 0.5))))))))))
;actual procedure:											    
(if  (d-Directive-score? "TransposeOnPrint")
    (begin
        (set! transpose (d-GetUserInput "Transposed Score" "Give transposition steps (e.g. -2 for down a second)" "3"))
        (if (and transpose (string->number transpose))
            (set! transpose (1- (string->number transpose)))
            (set! transpose 0))))
(d-PushPosition)
(while (d-PreviousMovement))
(AdjustFigures)
(while (d-NextMovement)
    (AdjustFigures))
(d-PopPosition))
