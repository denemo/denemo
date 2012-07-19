;;;;;;;;;;;;;InstrumentName
(let ((current "") (thematch "") (indent "0.0") (size 16.0))
 (if InstrumentName::params
 	(set! current InstrumentName::params)
  	(begin
  		(set! current (d-DirectiveGet-staff-postfix "InstrumentName" ))
  	(if (boolean? current)
    	  (set! current (_ "Violin"))
    	  (begin
		(set! thematch (string-match "\\\\set Staff.instrumentName = \"([^\"]*)\"" current))
		;;(display thematch)
		(if (regexp-match? thematch)
	   	 (set! current (match:substring thematch 1))
	    	(set! current "myname"))))
 		 (set! current (d-GetUserInput "InstrumentName" (_ "Give name of instrument/voice/part\nfor current staff:") current))))
 (if current
 	(begin
  		(d-DirectivePut-staff-display "InstrumentName" current)
  		(d-DirectivePut-staff-override "InstrumentName"  DENEMO_OVERRIDE_GRAPHIC)
  		(d-DirectivePut-staff-postfix "InstrumentName"  (string-append "\\set Staff.instrumentName = \"" current "\""))
  		(if (> (string-length current) 0)
  			(d-StaffProperties (string-append "denemo_name=" current))
  			(d-StaffProperties "denemo_name=unnamed"))
 		(set! size (/ (string->number (d-ScoreProperties "query=fontsize")) 10.0))
 		(set! indent (d-DirectiveGet-score-prefix "ScoreIndent"))
  		(if (boolean? indent)
      		(set! indent "0.0"))
     		 (begin
		(set! thematch (string-match "\\layout \\{indent = ([-0-9.]+)" indent))
		;;(display thematch)
		(if (regexp-match? thematch)
	   	 (set! indent (match:substring thematch 1))))
 		 (set! indent (number->string (max (string->number indent) (* size (string-length current)))))
   	 (d-DirectivePut-score-prefix "ScoreIndent" (string-append "\\layout {indent = " indent "}\n"))
  	(d-DirectivePut-score-override "ScoreIndent"	DENEMO_OVERRIDE_GRAPHIC)
 	 (d-DirectivePut-score-display "ScoreIndent" (string-append "indent=" indent))
	 (d-RefreshDisplay))))
