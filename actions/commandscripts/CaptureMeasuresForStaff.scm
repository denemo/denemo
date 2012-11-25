;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;CaptureMeasuresForStaff
(d-PushPosition)
(let loop ((measure-created #f))
	(if (d-UserScreenshot #t)
		(begin
			(d-AppendMeasure)			
			(loop #t))
		(begin
			(d-MoveToEnd)
			(if measure-created
			(d-DeleteMeasure)))))
(d-PopPosition)			
		