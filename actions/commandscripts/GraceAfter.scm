;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;GraceAfter
(d-PushPosition)
(if (Appending?)
  (d-MoveCursorLeft))
(while  (and (d-IsGrace) (d-MoveCursorLeft))
	#t)

(if (and (Note?) (not (d-IsGrace)))
    (begin
      (while (and (d-MoveCursorRight) (Note?) (d-IsGrace))
      	#t)
      (if (or (Appending?) (not (Note?)) (not (d-IsGrace)))
	  (d-MoveCursorLeft))
     (if (d-IsGrace)
     	(begin
     		 ;; on last grace note
   		 (let loop ((close "}"))
 		(if (d-IsGrace)
 			(let ((beam (d-DirectiveGet-chord-postfix "Beam")))
			   (if beam
			      (d-DirectivePut-chord-override "Beam" (logior (d-DirectiveGet-chord-override "Beam") DENEMO_OVERRIDE_HIDDEN))
		  	    (set! beam ""))		
	    		   (d-DirectivePut-chord-postfix "GraceAfter" (string-append (d-GetNote) (d-GetNoteDuration)  beam (if (d-IsSlurStart) "(" "") (if (d-IsSlurEnd) ")" "") close))
       		   	(d-DirectivePut-chord-override "GraceAfter" DENEMO_OVERRIDE_LILYPOND)
       		   	(if (d-MoveCursorLeft)
       		   		(loop "")))))
      	(d-MoveCursorRight)
     	 (d-DirectivePut-chord-prefix "GraceAfter" "{")
    	  (d-MoveCursorLeft)
    	  ; now on main note
    	  (d-DirectivePut-chord-display "MainGraceAfter"  "Grace After" )
     	 
 	   (d-DirectivePut-chord-prefix "MainGraceAfter"  "\\afterGrace ")
      	   (d-DirectivePut-chord-override "MainGraceAfter"  DENEMO_OVERRIDE_AFFIX)	
 	   (d-DirectivePut-chord-minpixels  "MainGraceAfter" 20)
	    (d-PopPosition)
	    (d-SetSaved #f))
  	(begin
   	   (d-PopPosition)
   	   (d-WarningDialog  (_ "The cursor is not on a grace note after a main note or chord\nFirst create the main note, then then follow it with the grace note(s) then invoke this command with the cursor on the grace note")))))
	(begin
   	  	 (d-PopPosition)
   	  	 (d-WarningDialog  (_ "The cursor is not on a grace note after a main note or chord\nFirst create the main note, then then follow it with the grace note(s) then invoke this command with the cursor on the grace note"))))