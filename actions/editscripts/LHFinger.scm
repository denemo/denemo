;;; tweak Fingering inserted by LHFinger

(let ((choice #f))
  (begin
    (set! choice (d-GetOption (string-append cue-OffsetPositionAll stop cue-SetPadding stop cue-SetRelativeFontSize stop cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))

     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-note "LHFinger"))
     ((equal? choice  cue-OffsetPositionAll)
      (ExtraOffset "Fingering"))
     ((equal? choice cue-SetRelativeFontSize)
      (SetRelativeFontSize "Fingering"))
     ((equal? choice cue-SetPadding)
      (SetPadding "Fingering")))))
(d-RefreshDisplay)
