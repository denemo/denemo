 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; PaperSize
(let ((size "A4") (orientation "portrait"))
  (set! size (d-GetOption  (string-append "a4" stop "letter" stop)))
  (set! orientation (d-GetOption  (string-append (_ "Portrait") stop (_ "Landscape") stop)))
  (if (equal? orientation (_ "Portrait"))
      (set! orientation " ")
      (set! orientation " 'landscape"))
  (d-DirectivePut-score-prefix "PaperSize" (string-append "#(set-default-paper-size \"" size "\"" orientation")\n"))
  (d-DirectivePut-score-override "PaperSize"	DENEMO_OVERRIDE_GRAPHIC)
  (d-DirectivePut-score-display "PaperSize" (string-append (_ "Paper Size =") size orientation))
  (d-RefreshDisplay))
