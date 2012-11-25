;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(ToggleDirective "note" "postfix" "Fingering" "-4 ")
(if (d-Directive-note? "Fingering")
	(d-DirectivePut-note-display"Fingering" "4"))
(d-Chordize #t)
