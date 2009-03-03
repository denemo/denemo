
;;; tweak  RehearsalMark

(let ((choice #f))
  (begin
    (set! choice (d-GetOption "Offset the Position\0Set Padding\0"))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice "Offset the Position")
      (ExtraOffset "RehearsalMark" "standalone" "Score."))
     ((equal? choice "Set Padding")
      (SetPadding "RehearsalMark" "standalone" "Score.")))))
(d-RefreshDisplay)
