(d-New)
(d-InfoDialog "Tutorial:  Dismiss this box and hit a Duration key 0, 1, 2, 3,4,5,6\nOr hit some other key to get on with creating a score")
(define thechar (d-GetChar))
(define thenumber (string->number thechar))
(if  thenumber
	(if (< thenumber 7)
		(begin
			(eval-string (string-append "(d-" thechar ")"))			
			(d-InfoDialog "Tutorial: You see a note of that duration\nAnd you hear a drum beat\nThe note is brown, because it has no pitch yet\nDismiss this box and hit a note name key a,b,c,d,e,f,g")				
			(set! thechar (d-GetChar))
			(if (string-contains "abcdefg" thechar)
				(begin
					(eval-string (string-append "(d-" (string-upcase thechar) ")"))
					(d-InfoDialog "Tutorial: The note has acquired a pitch\nDismiss this box and hit a few note name keys a,b,c,d,e,f,g finishing with some other letter, say x")
					(let loop () (set! thechar (d-GetChar))
						(if  (string-contains "abcdefg" thechar)
						(begin (eval-string (string-append "(d-" (string-upcase thechar) ")")) (loop))))
					(d-InfoDialog "Tutorial: The notes are entered with the same duration\nDismiss this box and change duration with a 0,1,2,3,4,5,6 keypress")
					(set! thechar (d-GetChar))
					(set! thenumber (string->number thechar))
					
					(if (< thenumber 7)
						(begin
						(eval-string (string-append "(d-" thechar ")"))			
						(d-InfoDialog "Tutorial: Again you see a note indicating the duration\nAnd you hear a drum beat so you can tell by ear which duration it is\nDismiss this box and hit a note name key a,b,c,d,e,f,g")				
						(set! thechar (d-GetChar))
						(if (string-contains "abcdefg" thechar)
							(begin
							(eval-string (string-append "(d-" (string-upcase thechar) ")"))
							(d-InfoDialog "Tutorial: Once more you can continue entering notes of that duration\nYou can also edit notes or their durations using the same keys. Use the shift key to switch between editing and inserting.\nDismiss this box and Press the key 'x' to finish.")
					(set! thechar (d-GetChar))	
				
					)))))
			
			))))
(if (equal? thechar "x")	
	(d-InfoDialog "Tutorial is Over!\nTo re-run it go to the Help menu, Tutorial\nSome useful keypresses: Use + and - for sharp and flat\nUse , and ' for up and down octave\nUse Ins for adding notes to a chord\nDismiss this box and consult Help menu, denemo.org and chat on irc freenode#denemo for more help")
	(d-InfoDialog "That wasn't the expected key\nThe tutorial will exit now.\nTo re-run it go to the Help menu, Tutorial\nConsult Help menu, or chat on irc freenode#denemo for more help"))


(d-SetSaved)
(d-New)