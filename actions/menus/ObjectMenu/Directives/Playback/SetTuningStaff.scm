 ;;;;;SetTuningForStaff
 (d-Directive-standalone "Tuning")
(d-DirectivePut-standalone-midibytes "Tuning" (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x0 0x0 0x0$ " (d-GetMidiTuning)  "0xf7"))
(d-DirectivePut-standalone-display "Tuning" (string-append (_ "This Staff: ") (d-GetTemperament) " " (d-GetFlattest) "-" (d-GetSharpest)))
(d-DirectivePut-standalone-ty "Tuning" -25)
(d-DirectivePut-standalone-minpixels "Tuning" 15)
(d-RefreshDisplay)

