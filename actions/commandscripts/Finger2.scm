(ToggleDirective "note" "postfix" "Fingering" "-2 ")
(if (d-Directive-note? "Fingering")
	(d-DirectivePut-note-display"Fingering" "2"))
(d-Chordize #t)
