 ;;;;;;;;;;;TwoGraceInsert with beaming into the score
 (Help::Push (cons 'doublestroketemp "Press a duration key to get two, beamed, grace notes"))

(let ()
(define command #f)
(define duration (d-GetCommand))
(cond
 ((equal? duration "d-0") (set! command d-0))
 ((equal? duration "d-1") (set! command d-1))
 ((equal? duration "d-2") (set! command d-2))
 ((equal? duration "d-3") (set! command d-3))
 ((equal? duration "d-4") (set! command d-4))
 ((equal? duration "d-5") (set! command d-5))
 ((equal? duration "d-6") (set! command d-6))
 ((equal? duration "d-7") (set! command d-7)))

(if command
   	(begin
		(command)
		(d-ToggleGrace)
		(d-StartBeam)
		(command)
		(d-ToggleGrace)
		(d-EndBeam)
		(Help::Pop))
  	(begin (Help::Pop) (Help::TimedNotice "To use this function correctly you need to give a duration.\nThis will insert two grace notes, beamed together if needed, with the given duration"))))
