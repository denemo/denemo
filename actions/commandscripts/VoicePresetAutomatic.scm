;;;VoicePresetAutomatic
(if (string=? (d-GetType) "Appending")
  (d-MoveCursorLeft))
(if (string=? (d-GetType) "STEMDIRECTIVE")
    (d-DeleteObject)
    (d-MoveCursorRight))
(d-InsertStem)
(d-MoveCursorLeft)
(d-DirectivePut-stemdirective-graphic "VoiceSetting" "
vAuto
Serif
24
1
1")
(d-DirectivePut-stemdirective-postfix "VoiceSetting" "\\oneVoice")
(d-DirectivePut-stemdirective-override "VoiceSetting" DENEMO_OVERRIDE_LILYPOND)
(d-MoveCursorRight)
(d-RefreshDisplay)
