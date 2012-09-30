;;DisplaceRestHorizontally
(let	( (X (d-GetUserInput (_ "Horizontal shift") (_ "Give horizontal shift required") "1.5")))
(if  X 
  (StandAloneDirectiveProto (cons "DisplaceRestHorizontally" (string-append  "\\once \\override Voice.Rest #'extra-offset = #'(" X " . 0.0)"  )) #t "\n⬌\nDenemo\n24")
  #f))
