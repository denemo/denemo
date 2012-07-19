 ;;; ParenthesizeChord
 (if (d-Directive-chord? "Parenthesize")
 	(d-DirectiveDelete-chord  "Parenthesize")
 	(begin
		(d-DirectivePut-chord-prefix "Parenthesize" "\\parenthesize ")
		(d-DirectivePut-chord-override "Parenthesize" DENEMO_OVERRIDE_AFFIX)
		(d-DirectivePut-chord-display "Parenthesize" "()")))
(d-SetSaved #f)