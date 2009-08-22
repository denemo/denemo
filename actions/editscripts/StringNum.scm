;;; tweak position of StringNumber inserted by StringNum
;;; note SetPadding and SetRelativeFontSize and ExtraOffset take the grob name "StringNumber", not the tag which is StringNum
;; FIXME common code with LHFinger.scm
(let ((choice #f))
  (begin
    (set! choice 
	  (d-GetOption (string-append cue-OffsetPositionAll stop cue-SetPadding stop cue-SetRelativeFontSize stop cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-note "StringNum"))
     ((equal? choice  cue-OffsetPositionAll)
      (ExtraOffset "StringNumber"))
     ((equal? choice cue-SetRelativeFontSize)
      (SetRelativeFontSize "StringNumber"))
     ((equal? choice cue-SetPadding)
      (SetPadding "StringNumber")))))
(d-RefreshDisplay)

