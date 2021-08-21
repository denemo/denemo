;;;TransposeScoreOnPlayback	
;relative to any transpose set on individual staffs
(let ((val 0))
	(d-PushPosition)
	(set! val (d-GetUserInput (_ "Transpose on Playback") (_ "Give semitones (+/-): ") "-2"))
	(if (and val (string->number val))
		(let ((script (string-append "(let* ((val " val ") 
										(this (string->number (d-StaffProperties \"query=transposition\")))
										(newval (+ this val)))
						(d-StaffProperties (string-append \"\\\"transposition=\" (number->string newval) \"\\\"\")))")))	 
				(ForEachStaffInScore script))
		(d-WarningDialog (_ "Cancelled")))
	(d-PopPosition))