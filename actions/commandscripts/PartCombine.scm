;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;PartCombine
(let ((tag "PartCombine"))
	(define (install-combine)
		(if (d-Directive-voice? tag)
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
				(d-MoveToVoiceDown)
				(d-DirectivePut-voice-prefix tag " } %{next voice is combined with previous part %} ")
				(d-DirectivePut-voice-display tag (_ "Part Combine "))
				(d-DirectivePut-voice-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))))
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
					(d-WarningDialog (_ "Only two voices can be part-combined"))
					(install-combine))
				(d-WarningDialog (_ "Must be on a staff with two voices"))))
	(d-PopPosition))
