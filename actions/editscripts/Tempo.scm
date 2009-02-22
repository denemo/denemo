;;; tweak position of Tempo indication, d-x and d-y are set by dragging in printview area.
(display "Editing position of tempo marking")
(let ((oldstr "")(start "")(end ""))
  (set! oldstr (d-DirectiveGet-chord-prefix "Text"))
  (if (equal? oldstr "")
      (set! oldstr #f))
  (set! start "\\once \\override TextScript  #'extra-offset = #'(")
  (set! end ")")
  (d-DirectivePut-chord-prefix "Text" (ChangeOffset oldstr start end))
  (d-RefreshDisplay))

