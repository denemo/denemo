;;;NoBeam
(if (> (d-GetNoteBaseDuration) 2)
  (let ((tag "NoBeam"))
    (if (d-Directive-chord? tag)
	(d-DirectiveDelete-chord tag)
	(begin
		(d-DirectivePut-chord-postfix tag "\\noBeam")
		(d-DirectivePut-chord-display tag "noBeam")
		(d-DirectivePut-chord-graphic tag "NoBeam")
		(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)))
  (d-SetSaved #f)
  (d-RefreshDisplay))
(d-WarningDialog (_ "No beam possible here")))
