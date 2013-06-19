;;UnGraceAfter
(d-PushPosition)
(if (Appending?)
  (d-MoveCursorLeft))
(while  (and (d-IsGrace) (d-MoveCursorLeft))
	#t)
(if (and (Note?) (d-Directive-chord? "MainGraceAfter"))
    (begin
      (while (and (d-MoveCursorRight) (d-IsGrace))
      	#t)
      (if (not (d-IsGrace))
	  (d-MoveCursorLeft))
      ;; on last grace note
      (let loop ()
 	(if (d-IsGrace)
 		(let ((beam (d-DirectiveGet-chord-postfix "Beam")))
		   (if beam
		      (d-DirectivePut-chord-override "Beam" 0);;really should just 0 the HIDDEN bit
		      )		
    		   (d-DirectiveDelete-chord "GraceAfter" )
       		   (if (d-MoveCursorLeft)
       		   	(loop)))))
      ; now on main note
       (d-DirectiveDelete-chord "MainGraceAfter")
       (d-PopPosition)
       (d-SetSaved #f))
    (begin
      (d-PopPosition)
      (d-WarningDialog  (_ "The cursor is not on a grace note after a main note or chord\n"))))
