;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(ToggleDirective "note" "postfix" "Fingering" "-0 ")
(if (d-Directive-note? "Fingering")
	(d-DirectivePut-note-display"Fingering" "0"))
(d-Chordize #t)
