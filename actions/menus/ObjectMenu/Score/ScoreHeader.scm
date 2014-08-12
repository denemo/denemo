;;;;;;;; ScoreHeader
(let ((tag "ScoreHeader") (prolog ""))
  (set! prolog (d-DirectiveGet-score-prefix tag))
  (if (not prolog)
      (set! prolog "%{Insert LilyPond syntax here %}\n"))
(set! prolog (d-GetUserInput (_ "LilyPond Score Header") (_ "Give LilyPond:") prolog))
(if prolog
    (begin
      (d-SetSaved #f)
      (d-DirectivePut-score-override tag DENEMO_OVERRIDE_AFFIX)
      (d-DirectivePut-score-prefix tag prolog)
      (d-DirectivePut-score-display tag prolog))
    (d-WarningDialog (_ "Prolog unchanged"))))