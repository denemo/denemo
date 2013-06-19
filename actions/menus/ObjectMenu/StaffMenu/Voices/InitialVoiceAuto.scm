;;;InitialVoiceAuto
(let ((tag "InitialVoice") (move-left #f))
(d-PushPosition)
(d-DirectiveDelete-voice tag)
(d-MoveToBeginning)
(if (StemDirective?) (begin (d-DeleteObject) (set! move-left #t)))
(d-PopPosition)
(if move-left
  (d-MoveCursorLeft));;to compensate for the deletion of the stem directive
(d-SetSaved #f))
