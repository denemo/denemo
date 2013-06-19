;;;;;;;; score prolog
(let ((prolog ""))
  (set! prolog (d-DirectiveGet-score-prefix "Prolog"))
  (if prolog 
      #t
      (set! prolog "%{Insert LilyPond syntax here %}\n"))
(set! prolog (d-GetUserInput (_ "LilyPond Score Prolog") (_ "Give LilyPond:") prolog))
(if prolog
    (begin
      (d-SetSaved #f)
      (d-DirectivePut-score-prefix "Prolog" prolog)
      (d-DirectivePut-score-display "Prolog" prolog))
    (d-WarningDialog (_ "Prolog unchanged"))))