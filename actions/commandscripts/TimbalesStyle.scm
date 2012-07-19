;;;;;;;;;; TimbalesStyle
(d-DirectivePut-staff-prefix "TimbalesStyle" "<< { \\new DrumStaff \\with {\n
      drumStyleTable = #timbales-style
      \\override StaffSymbol #'line-count = #2
      \\override BarLine #'bar-size = #2
  }\n")
(d-DirectivePut-staff-override "TimbalesStyle" 1)
(d-DirectivePut-staff-display "TimbalesStyle" "DrumStaff")
(d-DirectivePut-voice-postfix "TimbalesStyle" "\\drummode ")
(d-DirectivePut-voice-override "TimbalesStyle" 1)
(d-DirectivePut-clef-postfix "TimbalesStyle" "{ }\n ")
(d-DirectivePut-clef-override "TimbalesStyle" 1)
(d-DirectivePut-clef-graphic "TimbalesStyle" "DrumClef")
(d-DirectivePut-keysig-postfix "TimbalesStyle" "{ }\n ")
(d-DirectivePut-keysig-override "TimbalesStyle" 1)
(d-RefreshDisplay)