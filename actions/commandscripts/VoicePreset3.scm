;;;VoicePreset3
(if (string=? (d-GetType) "Appending")
  (d-MoveCursorLeft))
(if (string=? (d-GetType) "STEMDIRECTIVE")
    (d-DeleteObject)
    (d-MoveCursorRight))
(d-StartUpStems)
(d-MoveCursorLeft)
(d-DirectivePut-stemdirective-graphic "VoiceSetting" "
v3
Serif
24
1
1")
(d-DirectivePut-stemdirective-postfix "VoiceSetting" "\\voiceThree")
(d-DirectivePut-stemdirective-override "VoiceSetting" DENEMO_OVERRIDE_LILYPOND)
(d-MoveCursorRight)
(d-RefreshDisplay)
