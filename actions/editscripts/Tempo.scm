;;; tweak Tempo indication

(let ((choice #f))
  (begin
    (set! choice (d-GetOption (string-append cue-OffsetPositionAll stop cue-SetPadding stop cue-SetRelativeFontSize stop cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-chord "Tempo"))
     ((equal? choice  cue-OffsetPositionAll)
      (ExtraOffset "TextScript"))
     ((equal? choice cue-SetRelativeFontSize)
      (SetRelativeFontSize "TextScript"))
     ((equal? choice cue-SetPadding)
      (SetPadding "TextScript")))))
(d-RefreshDisplay)
