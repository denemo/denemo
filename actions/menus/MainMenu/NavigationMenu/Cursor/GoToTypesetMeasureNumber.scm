;;;GoToTypesetMeasureNumber
(let ((params GoToTypesetMeasureNumber::params)(off 0)(num (d-GetMeasureNumber)))
	(if (number? params)
		(set! num params)
		(set! num (d-GetUserInput (_ "Navigation") (_ "Give destination: ") (number->string (+ 10 num)))))
	(if (and num (string->number num))
		(begin
			(set! num (string->number num))
			(d-GoToPosition #f #f 1 1)
			(set! off (d-GetMeasureNumberOffset))
			(while (and (not (= num (+ off (d-GetMeasureNumber)))) (d-MoveToMeasureRight))
				(set! off (+ off (d-GetMeasureNumberOffset)))))))