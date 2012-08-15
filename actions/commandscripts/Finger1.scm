(ToggleDirective "note" "postfix" "Fingering" "-1 ")
(if (d-Directive-note? "Fingering")
	(d-DirectivePut-note-display"Fingering" "1"))
(d-Chordize #t)
