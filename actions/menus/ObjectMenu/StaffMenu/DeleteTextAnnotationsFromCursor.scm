;;;DeleteTextAnnotationsFromCursor
(let ((thunk (lambda () (d-DirectiveGetForTag-standalone "TextAnnotation"))))
	(while (d-NextObject)
		(if (thunk) (d-DeleteObject))))