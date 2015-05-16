;;;ExtendSlur
  (if (and (d-IsSlurEnd) (not (d-IsSlurStart)))
    (begin 
	(d-ToggleEndSlur)
	(d-NextChord)
	(d-ToggleEndSlur)
	(if (not (d-IsSlurEnd))
		(d-ToggleEndSlur)));;that is, either put the end slur back or put it on the next chord
   (begin
     (if (d-IsSlurStart)
     	(begin
     		(d-NextChord)
		(d-ToggleEndSlur))
   	(if (d-PrevChord)
   	 	(if (d-IsSlurEnd)
   		    (begin 
			(d-ToggleEndSlur)
			(d-NextChord)
			(d-ToggleEndSlur))
			(if (d-IsSlurStart)
			     	(begin
			     		(d-NextChord)
					(d-ToggleEndSlur))
		  		(d-NextChord)))))))
		

		
	