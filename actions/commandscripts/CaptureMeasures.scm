;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
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
		