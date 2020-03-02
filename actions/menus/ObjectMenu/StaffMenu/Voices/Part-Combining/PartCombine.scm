;;;PartCombine (no toggle when passed a parameter)
(let ((tag "PartCombine")(name PartCombine::params))
	(define (install-combine)
		(if (and (d-Directive-voice? tag) (not name))
			(begin
				(d-DirectiveDelete-voice tag)
				(d-MoveToVoiceDown)
				(d-DirectiveDelete-voice tag)
				(d-InfoDialog (_ "Parts are now uncombined")))
			(begin
				(d-DirectivePut-voice-prefix tag " \\set Staff.soloText = #\"\"
                                                  \\set Staff.soloIIText = #\"\"
                                                  \\set Staff.aDueText = #\"\" \\partcombine { ")
				(d-DirectivePut-voice-display tag (_ "Part Combine "))
				(d-DirectivePut-voice-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
				
				(if name (d-DirectivePut-voice-allow tag (d-GetIdForName name)))
				
				(d-MoveToVoiceDown)
				(d-DirectivePut-voice-prefix tag " } %{next voice is combined with previous part %} ")
				(d-DirectivePut-voice-display tag (_ "Part Combine "))
				(d-DirectivePut-voice-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
				
				(if name (d-DirectivePut-voice-allow tag (d-GetIdForName name)))))
		(d-SetSaved #f))

	(d-PushPosition)
	(if (d-MoveToVoiceDown)
		(if (d-MoveToVoiceDown)
			(d-WarningDialog (_ "Can only part-combine two voices"))
			(begin
				(d-MoveToVoiceUp)
				(install-combine)))
		(if (d-MoveToVoiceUp)
				(if (d-MoveToVoiceUp)
					(d-WarningDialog (_ "Can only part-combine two voices"))
					(install-combine))
				(d-WarningDialog (_ "Must be on a staff with two voices"))))
	(d-PopPosition))