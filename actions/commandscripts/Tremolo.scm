;;;Tremolo

(let ((command #f) (duration #f) (dur #f))
(set! command (lambda ()
		 (Help::TimedNotice "To use this function correctly you need to press a keyboard shortcut for a duration command\nThe duration will be used to divide the note at the cursor when printed.")))
(if (d-Directive-chord? "Tremolo")
	(d-DirectiveDelete-chord "Tremolo")
	(begin 
		(if (not DenemoKeypressActivatedCommand)
 			(Help::Push (cons 'doublestroketemp "Press a shortcut key 3-7 for the duration into which the note at the cursor is to be divided")))
   	  (set! duration (d-GetCommand))
          (set! dur #f)
   	  (cond
		 ((equal? duration "d-3") (set! dur ":8"))
		 ((equal? duration "d-4") (set! dur ":16"))
		 ((equal? duration "d-5") (set! dur ":32"))
		 ((equal? duration "d-6") (set! dur ":64"))
		 ((equal? duration "d-7") (set! dur ":128"))
 	     )
          (if (string? dur)
         	 (begin		
          		(d-DirectivePut-chord-display "Tremolo" "trem")
   			(d-DirectivePut-chord-postfix "Tremolo" dur))
	 	 (command))
	 	 (Help::Pop)
(d-RefreshDisplay))))
 
