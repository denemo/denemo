(let ((transpose 0))
(define (AdjustFigures)
    (define (GetNoteStaffPosition)
        (define height (d-GetNoteStaffPosition))
        (if height
                (+ transpose height)
                #f))
    (d-MoveToBeginning)
    (while (d-MoveToStaffDown))
    (while (d-NextNote)
        (let ((fig (d-GetBassFigure)) (beamed (if (> (d-GetNoteBaseDuration) 2) (* 1.3 (d-GetNoteBaseDuration)) #f)) (height  (GetNoteStaffPosition)))
            (define (multiple fig)
                (string-index fig #\|))
                
           (define (adjust height thelist)
                (define count 0)
                (if  (< height 4)
                    (set! height 4)) 
                 (while (pair? thelist)
                    (set! count (1+ count)) ;(disp "the list " thelist "\n")
                    (d-AdjustBassFigureHeight (cons (string-append "FBHeight" (number->string count)) (number->string (+  (* 1.75 (car thelist)) -1.250 (/ (+ 5 height) 2)))))
                    (set! thelist (cdr thelist))))
                    
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
                
            (define (figure-heights fig) ;; returns a list of the number of figures stacked vertically for each group in the string fig
                (define thelist '())
                (define num 1) ;the number of figures stacked vertically in the current group
                (define len (1- (string-length fig)))
                (let loop ((count  (1+ (skip-over-spaces fig 0 len))))
                    (define this (string-ref fig count)) ;(disp "looping with <" fig "> char " this " count" count " num =" num "\n")
                    (if (equal? this #\|)
                        (begin
                            (set! thelist (cons (1- num) thelist))
                            (set! count (skip-over-spaces fig (1+ count) len))
                            (set! num 1))
                        (if (equal? this #\space)
                            (begin
                                (set! num (1+ num))
                                (set! count (skip-over-spaces fig count len)))
                            (begin
                                (set! count (skip-over-figures fig count len)))))
                     (set! this  (string-ref fig count))
                    (if (and (= len 0)  (= num 1))
                            	(set! num (+ (if (equal? this #\+) 0.5 (if (equal? this #\-) 0.35 0)) num))) ;sharp and flat glyph are not on baseline
                    (if (< count len)
                        (loop (1+ count))
                        (set! thelist (cons num thelist))))
                (reverse thelist)) ;;;end create figure heights list
;;;procedure
           (if (and fig height (not (equal? fig "_")))
            (let ((thelist (figure-heights fig))(before 0)(after 0)(after-fig "~"))
                        (if (d-MoveCursorLeft)
                            (begin
                                (set! before (GetNoteStaffPosition))
                                (if (or (not before) (< before 5))
                                    (set! before height))
                                (d-MoveCursorRight)
                                (if (d-MoveCursorRight)
                                    (begin
                                        (set! after (GetNoteStaffPosition))
                                        (set! after-fig (d-GetBassFigure))
                                        (if (or (not after) (< after 5))
                                            (set! after height))
                                        (d-MoveCursorLeft)))))
                         (if (not (equal? after-fig "~"))
		                (if (and (> before height)(> after height)) ;this note is in a trough
		                    (begin ;(disp "the list " thelist "\n")
		                        (set! before (if (> before after) after before))
		                         (adjust before thelist))
		                     (begin
		                        (if (multiple fig)
		                            (begin
                                        (if beamed
                                            (set! height (+ beamed height)))
                                        (adjust (if (d-IsTied) (1+ height) height) thelist))))))))))) ;;;end of AdjustFigures for whole movement       
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
