;;ChoseMinimumStaffSpacing
(let ((value (d-GetUserInput (_ "Minimum Staff Spacing") (_ "Give Value ") "80")))
	(if (and value (string->number value))
		(let* ((val (string->number value)) (set-at  (d-SetMinimumStaffSpacing (if (> val 40) val 40))))
			(d-InfoDialog (string-append (_ "Staffs in the Display will be at least ") (number->string set-at) (_" pixels apart"))))
		(d-WarningDialog (_ "Cancelled"))))