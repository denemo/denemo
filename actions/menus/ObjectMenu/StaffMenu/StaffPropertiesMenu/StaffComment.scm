;;;;;;;;;;;;;;;;;;StaffComment
(let* ((tag "StaffComment")(current (d-DirectiveGet-staff-display tag)))
	(if (not current)
		(set! current (_ "comment for this staff")))
	(set! current (d-GetUserInput (_ "Staff Comment") (_ "Give comment text for current staff ") current #f))
	(d-DirectivePut-staff-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_EDITOR))
	(d-DirectivePut-staff-display tag current)
	(d-DisplayDirectiveTextEditor "staff" tag))