;;;;HiddenMeasure
(DenemoWholeMeasureRestCommand)
(d-MoveCursorLeft)
 (if (d-Directive-chord? DenemoWholeMeasureRestTag)
 	(begin
 		(d-DirectivePut-chord-display DenemoWholeMeasureRestTag "")
 		(d-DirectivePut-chord-display  "HiddenMeasure" (_ "Hidden Measure"))
 		(d-DirectivePut-chord-prefix "HiddenMeasure" "%{ ")
 		 (d-DirectivePut-chord-postfix "HiddenMeasure" "%} ")
 		 (d-DirectivePut-chord-graphic "HiddenMeasure" "\nH\nDenemo")
		(d-DirectivePut-chord-override "HiddenMeasure" DENEMO_OVERRIDE_LILYPOND)))
