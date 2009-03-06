;;; offer a delete

(let ((choice #f))
  (begin
    (set! choice (d-GetOption (string-append cue-Delete stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Delete)
      (d-DirectiveDelete-score "UnknownScoreTag")))))
(d-RefreshDisplay)
