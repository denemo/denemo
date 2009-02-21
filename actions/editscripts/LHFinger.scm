;;; tweak position of Fingering inserted by LHFinger, d-x and d-y are set by dragging in printview area.

(define oldstr (d-DirectiveGet-chord-prefix "LHFinger"))
(if (equal? oldstr "")
    (set! oldstr #f))
(define start "\\once \\override Fingering  #'extra-offset = #'(")
(define end ")")
(display oldstr)
(display start)
(display d-x)
(d-DirectivePut-chord-prefix "LHFinger" (ChangeOffset oldstr start end))
(d-RefreshDisplay)
