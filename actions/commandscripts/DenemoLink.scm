;;;DenemoLink
(if (d-Directive-standalone? "DenemoLink")
	(begin
		(FollowLink)
		(d-MoveCursorRight))
	(d-WarningDialog "There is no link here, open the source document and click on it to place one."))