;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
  ;;;ExtendSlur
  (if (d-IsSlurEnd)
  (begin 
(d-ToggleEndSlur)
(d-MoveCursorRight)
(d-ToggleEndSlur))
(begin
	(if (d-MoveCursorLeft)
		(if (d-IsSlurEnd)
		 (begin 
			(d-ToggleEndSlur)
			(d-MoveCursorRight)
			(d-ToggleEndSlur))
		(begin
		       (d-MoveCursorRight)	
	        	(d-ToggleBeginSlur)
			(d-MoveCursorRight)
			(d-ToggleEndSlur)
				)))))

	