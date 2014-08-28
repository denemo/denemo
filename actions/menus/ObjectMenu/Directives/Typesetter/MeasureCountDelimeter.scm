;;MeasureCountDelimeter
(let ((tag "MeasureCountDelimeter")(params MeasureCountDelimeter::params)(deftag "EnableMeasureCounter")(start #f))
    (if params
        (d-InfoDialog (_ "This directive instructs the LilyPond typesetter to start/stop placing a count above the measures."))
        (begin
            
            
            
            
            (if (not (d-Directive-score? deftag))
                (begin
                    (d-DirectivePut-score-prefix deftag  "
\\layout {
    \\context {
        \\Staff
        \\consists #Measure_counter_engraver
    }
}")
                (d-DirectivePut-score-display deftag deftag)
                (d-DirectivePut-score-override deftag DENEMO_OVERRIDE_AFFIX)))
            (d-PushPosition)
            (while (and (d-MoveCursorLeft) 
                        (not (d-Directive-standalone? tag))))
            (if (d-Directive-standalone? tag)
                    (set! start (eval-string (d-DirectiveGet-standalone-data tag))))
            (d-PopPosition)
            (if start
                (begin
                    (d-Directive-standalone tag)
                    (d-DirectivePut-standalone-data tag "#f")
                    (d-DirectivePut-standalone-display tag (_ "Stop Count"))
                    (d-DirectivePut-standalone-graphic tag "
]
Denemo
24")
                    (d-DirectivePut-standalone-postfix tag "\\stopMeasureCount"))
                (begin
                    (d-Directive-standalone tag)
                    (d-DirectivePut-standalone-data tag "#t")
                    (d-DirectivePut-standalone-display tag (_ "Start Count"))
                    (d-DirectivePut-standalone-graphic tag "
[
Denemo
24")
                    
                    (d-DirectivePut-standalone-postfix tag "\\startMeasureCount")))
            (d-DirectivePut-standalone-gy tag -50)
            (d-DirectivePut-standalone-minpixels tag 30)
            (d-SetSaved #f)
            (d-RefreshDisplay))))
            