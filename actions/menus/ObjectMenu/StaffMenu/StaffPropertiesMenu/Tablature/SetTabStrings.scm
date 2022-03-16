;;;SetTabStrings
(let ((tag "SetTabStrings")(tunings ""))
	(if (equal? (d-StaffType) "TabStaff")
		(if (Chord?)
			(begin
		  		 (d-DuplicateChord) ;;; to ensure chord has no duration in the LilyPond syntax got by (d-GetLilyPond)
		  		 (set! tunings (d-GetLilyPond))
		  		 (d-MoveCursorLeft)
		  		 (d-DeleteObject)
		  		 (d-DeleteObject)
		  		 (StandAloneDirectiveProto (cons tag (string-append "\\set Staff.stringTunings = \\stringTuning " tunings "\n")) #t "\nh"   )
		  		  (d-MoveCursorLeft)
		  		 (d-DirectivePut-standalone-display tag (string-append (_ "Open Strings: ") tunings)))
			(begin
				(if (d-Directive-standalone? tag)
					(begin
						
						(set! tunings (d-GetUserInput "Re-Entrant Tunings" "Re-order the strings" (d-DirectiveGet-standalone-display "SetTabStrings")))
						(if tunings
							 (begin
								(StandAloneDirectiveProto (cons tag (string-append "\\set Staff.stringTunings = \\stringTuning " tunings "\n")) #t "\nh"   )
								(d-MoveCursorLeft)
								(d-DirectivePut-standalone-display tag tunings))))
							(d-WarningDialog (_ "To use this command create a chord\nwith one note for each open string on your instrument\nand position the cursor on it")))))
		(d-WarningDialog (_ "Only useful in a TAB staff"))))
