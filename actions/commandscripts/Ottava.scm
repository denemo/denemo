;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;OttavaStart
(let ( (val (d-GetUserInput "Ottava" (_ "Give -1 for ottava bassa, 1 for ottava alta and 0 for end ottava") "0")))
	(if val
		(StandAloneDirectiveProto (cons "Ottava"   (string-append "\\ottava #" val " " )))))
		