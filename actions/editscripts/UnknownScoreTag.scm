;;; offer a delete or text edit

(let ((choice #f))
  (begin
    (set! choice (d-GetOption (string-append cue-Delete stop cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-score "UnknownScoreTag"))
     ((equal? choice  cue-Delete)
      (d-DirectiveDelete-score "UnknownScoreTag")))))
(d-RefreshDisplay)
