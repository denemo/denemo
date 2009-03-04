;;; tweak Fingering inserted by LHFinger

(let ((choice #f))
  (begin
    (set! choice (d-GetOption "Offset the Position\0Set Padding\0Set Relative Font Size\0"))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice "Offset the Position")
      (ExtraOffset "Fingering"))
     ((equal? choice "Set Relative Font Size")
      (SetRelativeFontSize "Fingering"))
     ((equal? choice "Set Padding")
      (SetPadding "Fingering")))))
(d-RefreshDisplay)
