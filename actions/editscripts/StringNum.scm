;;; tweak position of StringNumber inserted by StringNum, d-x and d-y are set by dragging in printview area.
(define oldstr (d-DirectiveGet-chord-prefix "StringNum"))
(if (equal? oldstr "")
    (set! oldstr #f))
(define start "\\once \\override StringNumber  #'extra-offset = #'(")
(define end ")")
(d-DirectivePut-chord-prefix "StringNum" (ChangeOffset oldstr start end))
(d-RefreshDisplay)


;;(d-DirectivePut-chord-prefix "StringNum"  "\\set fingeringOrientations = #'(left)")

;;;(d-DirectivePut-chord-prefix "StringNum"  "\\set fingeringOrientations = #'(left) \\override Fingering #'padding = #4")

;;\override Fingering #'font-size = #-7
;;\override Fingering #'padding = #4


;;;(d-DirectivePut-note-prefix "StringNum"  (string-append " \\override Score.Text  #'extra-offset = #'( " (number->string (exact->inexact (/ d-x 10.0))) " . " (number->string (exact->inexact (/ d-y 10.0))) " )"))
(d-RefreshDisplay)
