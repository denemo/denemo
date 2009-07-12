
;;; tweak  RehearsalMark

(let ((choice #f))
  (begin
    (set! choice (d-GetOption  (string-append "Offset the Position" stop "Set Padding" stop cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-standalone "RehearsalMark"))
     ((equal? choice "Offset the Position")
      (ExtraOffset "RehearsalMark" "standalone" "Score."))
     ((equal? choice "Set Padding")
      (SetPadding "RehearsalMark" "standalone" "Score.")))
    (if (not (boolean? choice))
	(d-RefreshDisplay))))

