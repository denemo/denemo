;;;CautionaryAccidental
(if (d-DirectiveGet-note-postfix "WarnAccidental")
		(d-DirectiveDelete-note "WarnAccidental")
		(begin
		(d-DirectivePut-note-postfix "WarnAccidental" "!")
		(d-DirectivePrioritizeTag-note "WarnAccidental")))
(d-SetSaved #f)
(d-RefreshDisplay)
