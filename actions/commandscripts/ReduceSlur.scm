;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
  ;;;ReduceSlur
  (if (d-IsSlurEnd)
  (begin 	
	(d-ToggleEndSlur)
	(d-MoveCursorLeft)
	(if (d-IsSlurStart)
		(d-ToggleBeginSlur)
	(d-ToggleEndSlur))))



