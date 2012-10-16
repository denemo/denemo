;;;CautionaryAccidental
(if (d-DirectiveGet-note-postfix "WarnAccidental")
	(d-DirectiveDelete-note "WarnAccidental")
(d-DirectivePut-note-postfix "WarnAccidental" "!"))
(d-SetSaved #f)
(d-RefreshDisplay)
