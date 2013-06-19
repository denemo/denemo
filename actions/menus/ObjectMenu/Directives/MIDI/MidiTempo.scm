;;; Tempo Change, Midi only.
(let ((DurationFromInput "1" ) (TempoFromInput "120"))
(set! DurationFromInput (d-GetOption (string-append "Quarter" stop "Dotted Quarter" stop)))
(set! TempoFromInput (d-GetUserInput "MidiTempo" "Please enter a tempo value" "120"))
(if TempoFromInput
(begin
(d-Directive-standalone "MidiTempo")
(d-DirectivePut-standalone-minpixels "MidiTempo" 20)
(d-DirectivePut-standalone-override "MidiTempo" (logior DENEMO_OVERRIDE_TEMPO DENEMO_OVERRIDE_STEP))

;Calculate the dotted tempo but display the userone
(if (string-ci=? DurationFromInput "Dotted Quarter" )
  (d-DirectivePut-standalone-midibytes "MidiTempo" (number->string (* 1.5 (string->number TempoFromInput))))
  (d-DirectivePut-standalone-midibytes "MidiTempo" TempoFromInput)
)
(format #t "Displaying  ~A ~%" (string-append DurationFromInput " = "  TempoFromInput) )
(d-DirectivePut-standalone-display "MidiTempo" (string-append DurationFromInput " = "  TempoFromInput))
(d-DirectivePut-standalone-ty "MidiTempo" -20)
)))

(d-RefreshDisplay)