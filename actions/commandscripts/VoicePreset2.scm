;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;VoicePreset2
(if (string=? (d-GetType) "Appending")
  (d-MoveCursorLeft))
(if (string=? (d-GetType) "STEMDIRECTIVE")
    (begin 
    	(d-MoveCursorRight)
    	(d-DeletePreviousObject)))
(d-StartDownStems)
(d-MoveCursorLeft)
(d-DirectivePut-stemdirective-graphic "VoiceSetting" "
v2
Serif
24
1
1")
(d-DirectivePut-stemdirective-postfix "VoiceSetting" "\\voiceTwo")
(d-DirectivePut-stemdirective-override "VoiceSetting" DENEMO_OVERRIDE_LILYPOND)
(d-MoveCursorRight)
(d-RefreshDisplay)
