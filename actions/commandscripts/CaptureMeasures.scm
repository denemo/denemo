;;;CaptureMeasures
(d-PushPosition)
(let loop ((measure-created #f))
	(if (d-UserScreenshot #f)
		(begin
			(d-AppendMeasureAllStaffs)			
			(loop #t))
		(begin
			(d-MoveToEnd)
			(if measure-created
			(d-DeleteMeasureAllStaffs)))))
(d-PopPosition)			
		