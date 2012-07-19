;;;;;;;;;;;;;;; MovementIndent Set indent

(let ((amount "0.0") (current "0.0")(thematch #f))
  (set! current (d-DirectiveGet-layout-postfix "MovementIndent"))
  (if (boolean? current)
      (set! current "15.0")
      (begin
	(set! thematch (string-match (string-append "indent = (.*)\n") current))
	(display thematch)
	(if (regexp-match? thematch)
	    (set! current (match:substring thematch 1)))))
  (set! amount (d-GetUserInput "Choose indent of first system" "Give amount as decimal" current))
  (display amount)
  (d-DirectivePut-layout-postfix "MovementIndent" (string-append "indent = " amount "\n")))
(d-SetSaved #f)
(d-RefreshDisplay)
;;;;;;;;;;;;;;;;;;;
