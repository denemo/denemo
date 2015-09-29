;;;;;;;;;;;;;;; ScoreIndent Set indent
(let ((tag "ScoreIndent")(amount #f) (current "0.0")(thematch #f)(params ScoreIndent::params))
  (set! current (d-DirectiveGet-score-data tag))
  (if (not current)
      (set! current "4.0"))
  (if (and params (not (equal? params "edit")))
    (set! amount (number->string params))
    (set! amount (d-GetUserInput (_ "Choose indent of first system") (_ "Give amount in decimal") current)))

  (if (and (string? amount) (string->number amount))
    (begin
        (d-DirectivePut-score-prefix tag (string-append "\\layout {indent = " amount "}\n"))
        (d-DirectivePut-score-override tag DENEMO_OVERRIDE_GRAPHIC)
        (d-DirectivePut-score-data tag amount)
        (d-DirectivePut-score-display tag (string-append (_ "indent=") amount)))
    (d-DirectiveDelete-score tag))
  (d-SetSaved #f))
