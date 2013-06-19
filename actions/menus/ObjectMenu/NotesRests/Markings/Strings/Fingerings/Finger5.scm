(ToggleDirective "note" "postfix" "Fingering" "-5 ")
(if (d-Directive-note? "Fingering")
	(d-DirectivePut-note-display"Fingering" "5"))
(d-Chordize #t)
