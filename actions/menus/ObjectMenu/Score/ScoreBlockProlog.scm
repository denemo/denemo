;;;;;;;; ScoreBlockProlog
(let ((prolog ""))
(if ScoreBlockProlog::params
 (set! prolog ScoreBlockProlog::params)
  (set! prolog (d-DirectiveGet-score-postfix "ScoreBlockProlog")))
  (if (not prolog)
      (set! prolog "%{Insert LilyPond syntax here %}\n"))
 (if (not ScoreBlockProlog::params)
	(set! prolog (d-GetUserInput "LilyPond Score Prolog" "Give LilyPond:" prolog)))
(if prolog
    (begin
      (d-SetSaved #f)
      (d-DirectivePut-score-postfix "ScoreBlockProlog" prolog)
      (d-DirectivePut-score-display "ScoreBlockProlog" prolog))
    (d-WarningDialog "Prolog unchanged")))