;;;InitialVoiceTwo
(let ((tag "InitialVoice") (move-right #f))
(d-PushPosition)
(d-DirectivePut-voice-display tag "Voice Two")
(d-DirectivePut-voice-postfix tag "\\voiceTwo")
(d-DirectivePut-voice-override tag DENEMO_OVERRIDE_GRAPHIC)
(d-MoveToBeginning)
(if (StemDirective?) (d-DeleteObject) (set! move-right #t))
(d-StartDownStems)
(d-MoveCursorLeft)
(d-DirectivePut-stemdirective-graphic tag "
v2
Serif
24
1
1")
(d-PopPosition)
(if move-right
  (d-MoveCursorRight));;to compensate for the insertion of the stem directive
(d-SetSaved #f))