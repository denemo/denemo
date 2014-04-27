;;ShiftNext
(let (( lilytype (GetTypeAsLilypond)))
	(if lilytype
		(let (( lilycontext  (GetContextAsLilypond))
			( X (d-GetUserInput (_ "X-Y shift") (_ "Give horizontal shift required") "1.5"))
			( Y  (d-GetUserInput (_ "X-Y shift") (_ "Give vertical shift required") "0.0")))
				(if (and X Y)
				  (StandAloneDirectiveProto (cons "ShiftNext" (string-append  "\\once \\override " lilycontext "." lilytype " #'extra-offset = #'(" X " . " Y")"  )))
				  #f))
		(d-WarningDialog (_ "This type of object cannot be shifted by this method"))))
