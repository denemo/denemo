;;; BeamLeftThree
(let ((tag "BeamLeft"))
(if (d-Directive-chord? tag)
	(d-DirectiveDelete-chord tag)
	(begin
	(d-DirectivePut-chord-prefix tag "\\set stemLeftBeamCount = #3 ")
	(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
	(d-DirectivePut-chord-display tag  "]3")))
(d-RefreshDisplay)
(d-SetSaved #f))
