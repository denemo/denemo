;;ShiftNext
(let (( lilytype (GetTypeAsLilypond))  
	( lilycontext  (GetContextAsLilypond))
	( X (d-GetUserInput (_ "X-Y shift") (_ "Give horizontal shift required") "1.5"))
	( Y  (d-GetUserInput (_ "X-Y shift") (_ "Give vertical shift required") "0.0")))
(if (and lilytype X Y)
  (StandAloneDirectiveProto (cons "ShiftNext" (string-append  "\\once \\override " lilycontext "." lilytype " #'extra-offset = #'(" X " . " Y")"  )))
  #f))
