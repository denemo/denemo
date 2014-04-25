;;DisplaceHorizontally
(let	( (tag "DisplaceHorizontally") (X (d-GetUserInput (_ "Horizontal shift") (_ "Give horizontal shift required") "0.5")))
(if  X 
   (begin
   	(StandAloneDirectiveProto (cons tag (string-append  "\\once \\override NoteColumn.X-offset = #" X " "  )) #f "\n⬌\nDenemo\n24" )
  	(d-DirectivePut-standalone-display tag X)
  	(d-DirectivePut-standalone-ty tag -20)
  	(d-MoveCursorRight)
  	(d-SetSaved #f)
  	(d-RefreshDisplay))))
