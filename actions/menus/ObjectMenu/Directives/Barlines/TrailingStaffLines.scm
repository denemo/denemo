;Trailing staff lines
(let ((tag "TrailingStaffLines"))
	(if (d-Directive-standalone? tag)
			(if (Appending?)
				(d-DeletePreviousObject)
				(d-DeleteObject))
			(begin
				(d-PushPosition)
				(GoToMeasureEnd)
				(if (d-MeasureRight)
					(d-WarningDialog (_ "Only useful at the end of a movmement"))
					(StandAloneDirectiveProto (cons tag " s8 ") #f "" (_ "Trailing staff lines")))
				(d-PopPosition))))