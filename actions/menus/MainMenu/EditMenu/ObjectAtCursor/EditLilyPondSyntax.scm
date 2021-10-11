;;;EditLilyPondSyntax - edits the syntax emitted for the object at the cursor
(let ((tag "EditLilyPondSyntax")(params EditLilyPondSyntax::params))
	(if (Music?)
		(d-DirectivePut-standalone-minpixels "Temp" 0)) ;force duration in lily syntax
	(if (d-Directive-standalone?)
		(begin
			(d-DirectiveTextEdit-standalone)
			(d-ChooseCondition))
		(let ((lily (d-GetLilyPond)))
			(if lily
				(let ((syntax #f))
					(set! syntax (d-GetUserInput (_ "Edit Current Object") (_ "Edit the LilyPond syntax") lily))
					(if syntax 
						(set! syntax (string-append " " syntax " ")))
					(if syntax
						(if (Music?)
							(begin
								(d-DirectivePut-chord-postfix tag syntax)
								(d-DirectivePut-chord-display tag syntax)
								(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_LILYPOND)
								(SetDirectiveConditional "chord" tag))
							(if (Clef?)
								(begin
									(d-DirectivePut-clef-postfix tag syntax)
									(d-DirectivePut-clef-display tag syntax)
									(d-DirectivePut-clef-override tag DENEMO_OVERRIDE_LILYPOND)
									(SetDirectiveConditional "clef" tag))
								(if (Timesignature?)
									(begin
										(d-DirectivePut-timesig-postfix tag syntax)
										(d-DirectivePut-timesig-display tag syntax)
										(d-DirectivePut-timesig-override tag DENEMO_OVERRIDE_LILYPOND)
										(SetDirectiveConditional "timesig" tag))
									(if (Keysignature?)
										(begin
										(d-DirectivePut-keysig-postfix tag syntax)
										(d-DirectivePut-keysig-display tag syntax)
										(d-DirectivePut-keysig-override tag DENEMO_OVERRIDE_LILYPOND)
										(SetDirectiveConditional "keysig" tag))))))
						  (d-WarningDialog (_ "Cancelled"))))		
				(d-WarningDialog (_ "No Object at Cursor")))))
	(if (Music?)
		(begin
			(d-MoveCursorLeft)
			(d-DirectiveDelete-standalone "Temp" ))))
					
			
