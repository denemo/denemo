;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(ToggleDirective "note" "postfix" "Fingering" "-5 ")
(if (d-Directive-note? "Fingering")
	(d-DirectivePut-note-display"Fingering" "5"))
(d-Chordize #t)
