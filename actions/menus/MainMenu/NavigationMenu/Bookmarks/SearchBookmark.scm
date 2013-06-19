(let ((user-input (d-GetUserInput "Find Bookmark" "What are you after? Case insensitive" "X")))
	(define start (GetPosition))
	(define (test?) 
		(FindNextObjectAllColumns (lambda () 
				(and (string? (d-DirectiveGet-standalone-display "RehearsalMark"))  ;Condition 1
				(string-ci=? (d-DirectiveGet-standalone-display "RehearsalMark") user-input))))) ; Condition 2
	;;Body
	(if user-input   ;in case the user pressed Escape do nothing
		(if (test?)
			#t ; Object found
			(begin  ; no object found from startpoint to end, return to movement beginning, search again.
				(d-MoveToMovementBeginning)
				(if (test?)
					#t ; Object found
					(begin (apply d-GoToPosition start) #f)))) ; no object even in the second round. Go to start position.
		#f)) ; user abort