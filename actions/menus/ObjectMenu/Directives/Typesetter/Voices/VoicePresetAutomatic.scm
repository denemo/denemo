;;;VoicePresetAutomatic
(let ((tag "VoiceSetting"))
(d-Directive-standalone tag)
(d-DirectivePut-standalone-minpixels tag 30)
(d-DirectivePut-standalone-graphic tag "
vAuto
Serif
24
1
1")
(d-DirectivePut-standalone-postfix tag "\\oneVoice ")
(d-MoveCursorRight)
(d-SetSaved #f)
(d-RefreshDisplay))
