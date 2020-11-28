;;;Ottava
(let ( (params Ottava::params) (val #f))
	(if (number? params)
		(set! val (number->string params))
		(set! val (d-GetUserInput "Ottava" (_ "Give -1 for ottava bassa, 1 for ottava alta and 0 for end ottava") "0")))
	(if (and val (string->number val))
		(StandAloneDirectiveProto (cons "Ottava"   (string-append "\\ottava #" val " ")) #t #f 
		(if (equal? val "0")
			(_ "End Ottava")
			(if (equal? val "1")
				"8va --------------"
				(if (equal? val "-1")
					"8va bassa -----------"
					(string-append "Ottava" val)))))))