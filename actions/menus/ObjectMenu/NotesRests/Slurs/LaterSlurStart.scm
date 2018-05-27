;;;LaterSlurStart
  (if (d-IsSlurStart)
    (begin 
    	(d-SetSaved #f)	
	(d-ToggleBeginSlur)
	(if (d-NextChord)
		(if (d-IsSlurEnd)
			(d-ToggleEndSlur)
			(d-ToggleBeginSlur)))))
  



