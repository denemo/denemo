;TransposeOnPlayback
(let ((params TransposeOnPlayback::params)(interval #f))
(define (do-staff interval)
	(d-StaffProperties (string-append "transposition="
			(number->string 
				(+  (string->number interval) 
					(string->number  (d-StaffProperties "query=transposition")))))))

(if (not params)
		(set! interval (d-GetUserInput (_ "Playback Transposition") (_ "Give semitones (+/-)") "-5")))
		
(if (number? interval)
	(set! interval (number->string interval)))
(if (string? interval)
	(begin
		(d-SetSaved #f)
		(while (d-StaffUp))
		(do-staff interval)
		(while (d-StaffDown)
			(do-staff interval)))
	(d-WarningDialog (_ "Cancelled"))))
