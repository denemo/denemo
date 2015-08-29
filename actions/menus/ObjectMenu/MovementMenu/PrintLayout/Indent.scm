;;;;;;;;;;;;;;; MovementIndent Set indent
(let ((tag "MovementIndent")(amount "0.0") (current "0.0")(thematch #f))
  (set! current (d-DirectiveGet-layout-data tag))
  (if (not current)
    (set! current "15.0"))
  (set! amount (d-GetUserInput (_ "Choose indent of first system") (_ "Give amount as decimal") current))
  (d-DirectivePut-layout-data tag amount)
  (d-DirectivePut-layout-postfix tag (string-append "indent = " amount "\n"))
  (d-DirectivePut-layout-display tag (string-append (_ "Indent= ") amount))
  (d-SetSaved #f))
  
