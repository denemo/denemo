;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;DenemoLink
(if (d-Directive-standalone? "DenemoLink")
	(begin
		(FollowLink)
		(d-MoveCursorRight))
	(d-WarningDialog (_ "There is no link here, open the source document and click on it to place one.")))