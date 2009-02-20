;;; tweak position, d-x and d-y are set by dragging in printview area.
(display d-x)
(display "and")
(display d-y)

(use-modules (ice-9 regex))

(define xy (string-append d-x " . " d-y))

(define oldstr (d-DirectiveGet-chord-prefix "StringNum"))

(define start "\\once \\override StringNumber  #'extra-offset = #'(")
(define end ")")
(if (boolean? oldstr)
      (define oldstr (string-append start " 0.0 . 0.0 " end)))
(define startbit (regexp-quote start))
(define endbit  (regexp-quote end))
(define theregexp (string-append startbit "[ ]*[-0-9.]+[ ]+.[ ]+[-0-9.]+[ ]*" endbit))
(define thematch (string-match theregexp oldstr))
(if (boolean? thematch)
    (begin
      (display "no match")
      )
    (begin 
      (define newstr (regexp-substitute #f thematch 'pre (string-append start xy end) 'post))
      (d-DirectivePut-chord-prefix "StringNum" newstr)
      ))
(display newstr)


;;(d-DirectivePut-chord-prefix "StringNum"  "\\set fingeringOrientations = #'(left)")

;;;(d-DirectivePut-chord-prefix "StringNum"  "\\set fingeringOrientations = #'(left) \\override Fingering #'padding = #4")

;;\override Fingering #'font-size = #-7
;;\override Fingering #'padding = #4


;;;(d-DirectivePut-note-prefix "StringNum"  (string-append " \\override Score.Text  #'extra-offset = #'( " (number->string (exact->inexact (/ d-x 10.0))) " . " (number->string (exact->inexact (/ d-y 10.0))) " )"))
(d-RefreshDisplay)
