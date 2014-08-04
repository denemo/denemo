;;;CheckBraces
(define CheckBraces::Return #f)
(define CheckBraces::ErrorPosition #f)
(define-once CheckScore::ignore 0)
(if (zero? CheckScore::ignore) 
    (let ((excess 0)
            (CP "ContextPianoStaff")
            (CG "ContextGrandStaff")
            (CC "ContextChoirStaff")
            (CR "ContextGroupStaff")
            (FirstError #f)
            (params CheckBraces::params)

            )

    (if (not params)
        (d-PushPosition))
    (while (d-MoveToStaffUp))
    (let loop () 
        (if (or (d-DirectiveGet-staff-prefix CP)
                (d-DirectiveGet-staff-prefix CG)
                (d-DirectiveGet-staff-prefix CC)
                (d-DirectiveGet-staff-prefix CR))
            (begin
                (set! excess (+ 1 excess))))
        (if (or (d-DirectiveGet-staff-postfix CP)
                (d-DirectiveGet-staff-postfix CG)
                (d-DirectiveGet-staff-postfix CC)
                (d-DirectiveGet-staff-postfix CR))
            (begin
                (if (zero? excess)
                    (set! FirstError (d-GetStaff)))
                (set! excess (- 1 excess))))
        (if (d-MoveToStaffDown)
                    (loop)))

    (if FirstError
        (set! CheckBraces::Return   (format #f "~a~a" (_ "Too few staff braces open at staff number ") FirstError))
        (if (not (zero? excess))
            (set! CheckBraces::Return   (_ "More staff braces started than are ended."))))

    (if (not params)
        (begin
            (d-PopPosition)
            (if CheckBraces::Return
                (d-WarningDialog CheckBraces::Return))))))
