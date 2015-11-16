(NotationMagick::ModifySelectedObjects
	 (lambda (selection) 
	 	(sort-list selection (lambda (x y) (< (list-ref (musobj.pitch x) 0) (list-ref (musobj.pitch y) 0))))))