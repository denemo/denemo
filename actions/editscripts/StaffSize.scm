;;; offer a delete

(let ((choice #f))
  (begin
    (set! choice (d-GetOption (string-append cue-Delete stop cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-staff "StaffSize"))
     ((equal? choice  cue-Delete)
      (d-SmallerStaff)))))
(d-RefreshDisplay)
