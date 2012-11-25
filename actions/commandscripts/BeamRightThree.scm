;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;; BeamRightThree
(let ((tag "BeamRight"))
(if (d-Directive-chord? tag)
	(d-DirectiveDelete-chord tag)
	(begin
	(d-DirectivePut-chord-prefix tag "\\set stemRightBeamCount = #3 ")
	(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
	(d-DirectivePut-chord-display tag  "[3")))
(d-RefreshDisplay)
(d-SetSaved #f))
