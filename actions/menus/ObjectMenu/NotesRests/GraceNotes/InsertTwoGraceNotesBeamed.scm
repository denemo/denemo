;;;;;;;;;;;TwoGraceInsert with beaming into the score
 ;(Help::Push (cons 'doublestroketemp (_ "Press a duration key to get two, beamed, grace notes")))
(Help::Push (cons 'doublestroketemp
(string-append " <span font_desc=\"22\" foreground=\"blue\">"  (_ "Press a duration key to get two, beamed, grace notes")  "</span>")))
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
   	(let ((middle(not (Appending?))))
		(command)
		(if middle
			(d-MoveCursorLeft))
		(d-ToggleGrace)
		(d-StartBeam)
		(if middle
			(d-MoveCursorRight))
		(command)
		(if middle
			(d-MoveCursorLeft))
		(d-ToggleGrace)
		(d-EndBeam)
		(Help::Pop))
  	(begin (Help::Pop) (Help::TimedNotice (_ "To use this function correctly you need to give a duration.\nThis will insert two grace notes, beamed together if needed, with the given duration")))))
