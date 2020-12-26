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
             (define (skip-over-spaces fig count len)
                (let loop ()
                    (define this (string-ref fig count))
                    (if (and (equal? this #\space) (< count (string-length fig)))
                        (begin
                            (set! count (1+ count))
                            (if (< count len)
                                (loop)))))
                (1- count))
                
            (define (skip-over-figures fig count len)
                (let loop ()
                    (define this (string-ref fig count))
                    (if (not (or (equal? this #\space)(equal? this #\|)))
                        (begin
                            (set! count (1+ count))
                            (if (< count len)
                                (loop)))))
                (1- count))
                
            (define (figure-height fig) ;;returns a spacing for highest number of figures stacked in fig
                (define highest 0)
                (define num 1) ;the number of figures stacked vertically in the current group
                (define len (1- (string-length fig)))
                (let loop ((count  (1+ (skip-over-spaces fig 0 len))))
                    (define this (string-ref fig count)) ;(disp "looping with <" fig "> char " this " count" count " num =" num "highest " highest "\n")
                    (if (equal? this #\|)
                        (begin
                            (set! count (skip-over-spaces fig (1+ count) len))
                            (set! num 1))
                        (if (equal? this #\space)
                            (begin
                                (set! num (1+ num))
                                (set! count (skip-over-spaces fig count len)))
                            (begin
								(set! highest (max (1- num) highest))
                                (set! count (skip-over-figures fig count len)))))  
                    (set! this  (string-ref fig count))
                    (if (and (= len 0)  (= num 1))
                            	(set! num (+ (if (equal? this #\+) 0.5 (if (equal? this #\-) 0.35 0)) num))) ;sharp and flat glyph are not on baseline  	
                    (if (< count len)
                        (loop (1+ count))))
                (* 3.5 highest)) ;;;end find highest
			(define (GetNoteStaffPosition)
				(define height (d-GetNoteStaffPosition))
				(if height
						(+ transpose height)
						#f))
				(d-MoveToBeginning)
				(while (d-MoveToStaffDown))
				(while (d-NextNote)
					(let ((fig (d-GetBassFigure)) (ornament-height (OrnamentHeight))(height  (GetNoteStaffPosition)))
					
						(set! height (+ height (figure-height fig)))
				   
						(if (and fig height (positive? ornament-height) (> height 1))
								(d-AdjustBassFigureHeight (cons "FBHeight1" 
									(number->string (if (< height 4) (+ ornament-height 5.75)
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
