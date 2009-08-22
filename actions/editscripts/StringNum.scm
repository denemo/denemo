;;; tweak position of StringNumber inserted by StringNum
;; FIXME common code with LHFinger.scm
(let ((choice #f))
  (begin
    (set! choice 
	  (d-GetOption (string-append cue-OffsetPositionAll stop cue-SetPadding stop cue-SetRelativeFontSize stop cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-note "StringNumber"))
     ((equal? choice  cue-OffsetPositionAll)
      (ExtraOffset "StringNumber"))
     ((equal? choice cue-SetRelativeFontSize)
      (SetRelativeFontSize "StringNumber"))
     ((equal? choice cue-SetPadding)
      (SetPadding "StringNumber")))))
(d-RefreshDisplay)












;;(ExtraOffset "StringNumber")
;;(d-RefreshDisplay)

;;(d-DirectivePut-chord-prefix "StringNum"  "\\set fingeringOrientations = #'(left)")

;;;(d-DirectivePut-chord-prefix "StringNum"  "\\set fingeringOrientations = #'(left) \\override Fingering #'padding = #4")

;;\override Fingering #'font-size = #-7
;;\override Fingering #'padding = #4
