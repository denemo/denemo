;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;VoicePreset4
(if (string=? (d-GetType) "Appending")
  (d-MoveCursorLeft))
(if (string=? (d-GetType) "STEMDIRECTIVE")
    (d-DeleteObject)
    (d-MoveCursorRight))
(d-StartDownStems)
(d-MoveCursorLeft)
(d-DirectivePut-stemdirective-graphic "VoiceSetting" "
v4
Serif
24
1
1")
(d-DirectivePut-stemdirective-postfix "VoiceSetting" "\\voiceFour")
(d-DirectivePut-stemdirective-override "VoiceSetting" DENEMO_OVERRIDE_LILYPOND)
(d-MoveCursorRight)
(d-RefreshDisplay)
