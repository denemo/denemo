;;;; ScoreShortIndent Set indent for subsequent systems
(let ((tag "ScoreShortIndent")(amount #f) (current "0.0")(thematch #f)(params ScoreShortIndent::params))
  (set! current (d-DirectiveGet-score-data tag))
  (if (not current)
      (set! current "4.0"))
  (if (and params (not (equal? params "edit")))
    (set! amount (number->string params))
    (set! amount (d-GetUserInput (_ "Choose indent of subsequent systems") (_ "Give amount in decimal") current)))

  (if (and (string? amount) (string->number amount))
    (begin
        (d-DirectivePut-score-prefix tag (string-append "\\layout {short-indent = " amount "}\n"))
        (d-DirectivePut-score-override tag DENEMO_OVERRIDE_GRAPHIC)
        (d-DirectivePut-score-data tag amount)
        (d-DirectivePut-score-ignore tag (d-GetIdForName (d-StaffProperties "query=lily_name"))) 
        (d-DirectivePut-score-display tag (string-append (_ "short indent=") amount)))
    (d-DirectiveDelete-score tag))
  (d-SetSaved #f))
