;;;PitchShift
(let ((val (d-GetUserInput (_ "Pitch Change") (_ "Give amount =/-") "10")))
(if val
	(begin 
		(set! val (string->number val))
		(set! val (+ 66 val))
		(if (> val 127)
			(set! val 127))
		(if (< val 0)
			(set! val 0))
		(set! val (number->string val))
		(let loop ((channel 0))
			(if (< channel 16)
				(let ((chan (number->string  (+ #xE0 channel))))
					(d-OutputMidiBytes (string-append chan " 0x00 " val))
					(loop (+  channel 1))))))))
