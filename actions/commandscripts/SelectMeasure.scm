;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(if (None?)
	(d-SetMark)
	(begin
		(d-PushPosition)
		(d-UnsetMark)
		(GoToMeasureBeginning)
		(RepeatProcWhileTest d-CursorRight (lambda () (not (Appending?))))
		(d-PopPosition)))