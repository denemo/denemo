;;;SetTablatureTunings
(let ((tunings ""))
(if (Chord?)
	(begin
  		 (d-DuplicateChord) ;;; to ensure chord has no duration in LilyPond
  		 (set! tunings (d-GetLilyPond))
  		 (d-DeleteObject)
  		 (d-MoveCursorLeft)
  		 (StandAloneDirectiveProto (cons "SetTabStrings" (string-append "\\set Staff.stringTunings = \\stringTuning " tunings "\n")) #t "\nh"   )
  		  (d-MoveCursorLeft)
  		 (d-DirectivePut-standalone-display "SetTabStrings" tunings)
   		  (d-TabStaff))
	(begin
		(if (d-Directive-standalone "SetTabStrings")
			(begin
				
				(set! tunings (d-GetUserInput "Re-Entrant Tunings" "Re-order the strings" (d-DirectiveGet-standalone-display "SetTabStrings")))
				(if tunings
					 (begin
						(StandAloneDirectiveProto (cons "SetTabStrings" (string-append "\\set Staff.stringTunings = \\stringTuning " tunings "\n")) #t "\nh"   )
						(d-MoveCursorLeft)
						(d-DirectivePut-standalone-display "SetTabStrings" tunings))))
				
				
					(d-WarningDialog "To use this command create a chord\nwith one note for each open string on your instrument\nand position the cursor on it")))))
