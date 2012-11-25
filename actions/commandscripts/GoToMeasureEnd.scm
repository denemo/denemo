;;; Warning!!! This file is derived from those in actions/menus/... do not edit here

(let loop ()
    (if  (or (None?) (Appending?))
	#t
	(begin (d-MoveCursorRight) (loop))))
		