;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;; BeamLeftOne
(let ((tag "BeamLeft"))
(if (d-Directive-chord? tag)
	(d-DirectiveDelete-chord tag)
	(begin
	(d-DirectivePut-chord-prefix tag "\\set stemLeftBeamCount = #1 ")
	(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
	(d-DirectivePut-chord-display tag  "]1")))
(d-RefreshDisplay)
(d-SetSaved #f))
