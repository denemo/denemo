;;;;;;;;Beam1234
(if (d-Directive-standalone? "Beam1234")
	(d-DirectiveDelete-standalone "Beam1234")
	(StandAloneDirectiveProto (cons "Beam1234"  
  "
#(override-auto-beam-setting '(end * * * *) 1 4)
#(override-auto-beam-setting '(end * * * *) 2 4)
#(override-auto-beam-setting '(end * * * *) 3 4)
#(override-auto-beam-setting '(end * * * *) 4 4)")))

;;(d-MoveCursorRight)
;;(d-RefreshDisplay)))
