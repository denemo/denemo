;;ShiftNext
(let (( lilytype (GetTypeAsLilypond))  
	( lilycontext  (GetContextAsLilypond))
	( X (d-GetUserInput "X-Y shift" "Give horizontal shift required" "1.5"))
	( Y  (d-GetUserInput "X-Y shift" "Give vertical shift required" "0.0")))
(if (and lilytype X Y)
  (StandAloneDirectiveProto (cons "ShiftNext" (string-append  "\\once \\override " lilycontext "." lilytype " #'extra-offset = #'(" X " . " Y")"  )))
  #f))
