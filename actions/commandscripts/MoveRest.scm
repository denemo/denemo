;;;MoveRest
(if (d-Directive-chord? "PolyphonicRest")
	(begin
		(d-DirectiveDelete-chord "PolyphonicRest")
		(d-StagedDelete)))
(if (Rest?)
  (let ((which  (duration::lilypond->denemo (string->number (d-GetNoteDuration)))))
    (d-AddNoteToChord)
    (d-DirectivePut-chord-postfix "PolyphonicRest" "\\rest")
    (d-DirectivePut-chord-graphic  "PolyphonicRest" (vector-ref Rests which))
    (d-DirectivePut-chord-override "PolyphonicRest" (logior DENEMO_OVERRIDE_VOLUME DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
  ))
(d-SetSaved #f)
