;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ReminderAccidental
(if (d-DirectiveGet-note-postfix "WarnAccidental")
	(d-DirectiveDelete-note "WarnAccidental")
	(d-DirectivePut-note-postfix "WarnAccidental" "?"))
(d-RefreshDisplay)
(d-SetSaved #f)
