;;;AdvancedEdit
(if (d-Directive-standalone?)
	(if  (not (d-DirectiveTextEdit-standalone))
		(d-DeleteObject))
	(d-InfoDialog (_ "To edit a directive attached to the object at the cursor\nright click and choose Run Object Editor")))
