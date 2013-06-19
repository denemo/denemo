
(let loop ()
    (if  (or (None?) (Appending?))
	#t
	(begin (d-MoveCursorRight) (loop))))
		