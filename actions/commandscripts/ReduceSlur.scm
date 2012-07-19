  ;;;ReduceSlur
  (if (d-IsSlurEnd)
  (begin 	
	(d-ToggleEndSlur)
	(d-MoveCursorLeft)
	(if (d-IsSlurStart)
		(d-ToggleBeginSlur)
	(d-ToggleEndSlur))))



