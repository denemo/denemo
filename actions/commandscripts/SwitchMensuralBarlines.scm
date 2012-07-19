; Mensural Barlines // "Mensurstriche" layout for one movement
; Consist of three, technically independent, Lilypond Directives.
(if (and (d-DirectiveGet-layout-postfix "MensuralBarlines")  (d-DirectiveGet-score-prefix "MensuralBarlines")  (d-DirectiveGet-movementcontrol-postfix "MensuralBarlines"))
	(begin ;If there is a directive, delete all
		(d-DirectiveDelete-layout "MensuralBarlines")
		(d-DirectiveDelete-score "MensuralBarlines")
		(d-DirectiveDelete-movementcontrol "MensuralBarlines")
	)
	(begin   ;If there is no directive present, create one
		; Make barlines transparent only within the staff-lines range
		(d-DirectivePut-layout-postfix "MensuralBarlines" " 
		\\context {
		    \\Score
		    \\override BarLine #'transparent = ##t
		  }
		")

		; Override the automatically placed \EndMovementBarline with a explicitly drawn end-barline.
		; This will enable the final Mensural Barline, but will not disturb other movements.
		(d-DirectivePut-score-prefix "MensuralBarlines" "
		EndMovementBarline = { \\once \\override Score.BarLine #'transparent = ##f \\bar \"|.\" }
		")
		
		; Surround all Staffs with a StaffGroup, which draws the barlines between the staffs. 
		(d-DirectivePut-movementcontrol-postfix "MensuralBarlines" "\\score { \\new StaffGroup   \n")
		(d-DirectivePut-movementcontrol-override "MensuralBarlines" DENEMO_OVERRIDE_LILYPOND)			
	)
)