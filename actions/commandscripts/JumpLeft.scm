;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(if (d-PrevObjectInMeasure) 
	(if (d-MoveToMeasureLeft) (d-MoveToMeasureRight))
	(d-MoveToMeasureLeft))