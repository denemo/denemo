;MoveToEarliestEmptyMeasure
(RepeatUntilFail (lambda () (and (None?) (d-MoveToMeasureLeft))))
(if (not (None?)) (d-MoveToMeasureRight))