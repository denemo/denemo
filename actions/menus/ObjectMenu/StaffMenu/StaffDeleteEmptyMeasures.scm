(while (d-MoveToMeasureRight)
	(if (EmptyMeasure?)
		(begin
		(d-DeleteMeasure)
		(d-MoveToMeasureLeft))))