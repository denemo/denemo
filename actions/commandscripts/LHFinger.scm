;;;;;;;;;;;;; d-LHFinger
(let ((num "0"))
(set! num (d-GetKeypress))
(d-DirectivePut-note-display "LHFinger" num)
(d-DirectivePut-note-postfix "LHFinger"  (string-append "-" num))
(d-DirectivePut-note-minpixels "LHFinger" 20)
(d-DirectivePut-note-tx  "LHFinger" 10)
(d-Chordize #t)
(d-RefreshDisplay))