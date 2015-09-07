;;;WholeMeasureRepeat
(if (d-Directive-score? "CompactChordChart")
	 (let ((tag "WholeMeasureRepeat")(timesig (GetPrevailingTimeSig #t)))
	    (d-InsertWholeRest)
	    (d-SetDurationInTicks (* 1536 timesig))
	     (d-DirectivePut-chord-override tag (logior DENEMO_OVERRIDE_LILYPOND DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
	    (d-DirectivePut-chord-gy tag 15)
	    (d-DirectivePut-chord-graphic tag "
𝄎
Denemo
48")
	   (d-DirectivePut-chord-postfix tag (string-append "s1*" (number->string timesig)
	 "
 -\\tweak #'extra-offset  #'(5 . 2)
-\\markup \\scale #'(0.5 . -0.5){ 
\\translate-scaled #'(4 . 0.5)    \\draw-circle #1 #0 ##t 
\\translate-scaled #'(-2 . 4) \\beam #4 #-1.5 #2   \\translate-scaled #'(0 . 0.5)
\\draw-circle #1 #0 ##t }    "))
	   (d-SetObjectDisplayWidth 100)
	    (d-SetSaved #f))
	(let ()
	(d-WholeMeasureRest)
	 (d-DirectivePut-chord-gx "WholeMeasureRest" 40)
	 (d-DirectivePut-chord-gy "WholeMeasureRest" 15)
	 (d-DirectivePut-chord-display "WholeMeasureRest" (_ "Measure Repeat"))
	(d-DirectivePut-chord-graphic "WholeMeasureRest" "
𝄎
Denemo
48")

	(d-DirectivePut-chord-prefix "WholeMeasureRepeat" "\\once \\override MultiMeasureRest #'extra-offset = #'(0 . -1)  \\override MultiMeasureRest #'stencil  = #ly:multi-measure-rest::percent  \\override MultiMeasureRest #'thickness = #0.48 ")
	(d-SetSaved #f)))