;;;LaterSlurStart
  (if (d-IsSlurStart)
    (begin 
    	(d-SetSaved #f)	
	(d-ToggleStartSlur)
	(if (d-NextChord)
		(if (d-IsSlurEnd)
			(d-ToggleEndSlur)
			(d-ToggleBeginSlur)))))
  



