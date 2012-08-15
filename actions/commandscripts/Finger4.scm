(ToggleDirective "note" "postfix" "Fingering" "-4 ")
(if (d-Directive-note? "Fingering")
	(d-DirectivePut-note-display"Fingering" "4"))
(d-Chordize #t)
