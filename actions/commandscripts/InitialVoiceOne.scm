;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;InitialVoiceOne
(let ((tag "InitialVoice") (move-right #f))
(d-PushPosition)
(d-DirectivePut-voice-display tag (_ "Voice One"))
(d-DirectivePut-voice-postfix tag "\\voiceOne")
(d-DirectivePut-voice-override tag DENEMO_OVERRIDE_GRAPHIC)
(d-MoveToBeginning)
(if (StemDirective?) (d-DeleteObject) (set! move-right #t))
(d-StartUpStems)
(d-MoveCursorLeft)
(d-DirectivePut-stemdirective-graphic tag "
v1
Serif
24
1
1")
(d-PopPosition)
(if move-right
  (d-MoveCursorRight));;to compensate for the insertion of the stem directive
(d-SetSaved #f))