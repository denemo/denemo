;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(NotationMagick::ModifySelectedObjects
	 (lambda (selection) 
	 	(sort-list selection (lambda (x y) (< (list-ref (musobj.pitch x) 0) (list-ref (musobj.pitch y) 0))))))