;;; tweak values of D.C. Al fineindication, d-x and d-y are set by dragging in printview area.

(let ((choice #f))
  (begin
    (set! choice (d-GetOption "Edit position\0Edit text\0"))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice "Edit text")
      (d-WarningDialog "Not yet implemented"))	
     ((equal? choice "Edit position")
      (ExtraOffset "TextScript")))
    (d-RefreshDisplay)))
