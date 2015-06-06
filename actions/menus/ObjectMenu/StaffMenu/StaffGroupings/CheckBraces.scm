;;;CheckBraces
(define CheckBraces::Return #f)
(define CheckBraces::ErrorPosition #f)
(define-once CheckScore::ignore 0)
(if (zero? CheckScore::ignore) 
    (let ((excess 0)
            (CP "PianoStaffStart")
            (CG "GrandStaffStart")
            (CC "ChoirStaffStart")
            (CR "GroupStaffStart")
            (EB "BraceEnd")
            (FirstError #f)
            (params CheckBraces::params)
	   (endbraces "0")
            )

    (if (not params)
        (d-PushPosition))
    (while (d-MoveToStaffUp))
    (let loop () (disp "excess so far " excess "\n")
        (if (or (d-Directive-staff? CP)
                (d-Directive-staff? CG)
                (d-Directive-staff? CC)
                (d-Directive-staff? CR))
            (begin
                (set! excess (+ 1 excess))))
        (if (d-Directive-staff? EB)
            (begin
            	(set! endbraces (d-DirectiveGet-staff-data EB))
                (if (zero? excess)
                    (set! FirstError (d-GetStaff)))
                (set! excess (- excess  (string->number endbraces)))))
        (if (d-MoveToStaffDown)
                    (loop)))

    (if FirstError
        (set! CheckBraces::Return   (format #f "~a~a" (_ "Too few staff braces open at staff number ") FirstError))
        (if (> excess 0)
            (set! CheckBraces::Return   (_ "More staff braces started than are ended."))
            (if (< excess 0)
            	(set! CheckBraces::Return   (_ "More staff braces ended than are started.")))))

    (if (not params)
        (begin
            (d-PopPosition)
            (if CheckBraces::Return
                (d-WarningDialog CheckBraces::Return))))))
