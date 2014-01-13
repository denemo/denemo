;;;MMRExpandLimit
(let ((tag "MMRExpandLimit") (limit "10"))
   (set! limit (d-GetUserInput (_ "Multi-Measure Rests") (_ "Give limit for use of church rests") limit))
  (if (and limit (string->number limit))
	(begin
		(d-DirectivePut-standalone tag)
		(d-DirectivePut-standalone-display tag (_ "MMR Limit"))
		(d-DirectivePut-standalone-postfix tag 
			(string-append "\\override MultiMeasureRest #'expand-limit = #" limit))
		(d-DirectivePut-standalone-minpixels tag 30)
		(d-RefreshDisplay))))