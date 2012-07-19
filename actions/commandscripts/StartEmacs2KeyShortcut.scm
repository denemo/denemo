;;;;;;;;;;;;;; emacs commands

(define first (d-GetCommandKeypress))
(define second (d-GetKeypress))
(define command (lambda ()
            (d-GetUserInput (string-append first ", " second) "Undefined combination." "OK")
	    ))

(define cancel  (lambda ()
            (d-GetUserInput first "Cancelled." "OK")
	    ))

(cond
 ((equal? first "<Control>x") (cond
  ((equal? second "s") (set! command d-Save))
 ((equal? second "k") (set! command d-Close))
 ((equal? second "<Control>g") (set! command cancel))
 ))
 ((equal? first "<Control>c") (cond
 ((equal? second "<Contro>g") (set! command cancel))
 ))
 )	  
(command)
