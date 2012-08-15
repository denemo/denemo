;;;;;;;;;;;;;ShortInstrumentName
(let ((current "") (thematch ""))
  (set! current (d-DirectiveGet-staff-postfix "ShortInstrumentName" ))
  (if (boolean? current)
      (set! current "Vln")
      (begin
	(set! thematch (string-match "\\\\set Staff.ShortInstrumentName = \"([^\"]*)\"" current))
	;;(display thematch)
	(if (regexp-match? thematch)
	    (set! current (match:substring thematch 1))
	    (set! current "Vln"))))
  (set! current (d-GetUserInput "ShortInstrumentName" "Give Short Instrument Name:" current))
  (if current
  	(begin 
  		(d-DirectivePut-staff-display "ShortInstrumentName" current)
  		(d-DirectivePut-staff-override "ShortInstrumentName"  DENEMO_OVERRIDE_GRAPHIC)
  		(d-DirectivePut-staff-postfix "ShortInstrumentName"  (string-append "\\set Staff.shortInstrumentName = \"" current "\""))
  		(d-SetSaved #f))))

