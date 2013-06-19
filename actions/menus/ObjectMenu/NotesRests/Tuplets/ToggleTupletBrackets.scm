;;;tuplet Brackets toggle
(let ((current ""))
  (set! current (d-DirectiveGet-standalone-postfix "ToggleTupletBrackets"))
  (if (boolean? current)
      (begin
	(d-DirectivePut-standalone-minpixels "ToggleTupletBrackets" 20)
	(d-MoveCursorLeft)
	(set! current  "\\override TupletBracket #'bracket-visibility = ##f")
	(d-DirectivePut-standalone-postfix "ToggleTupletBrackets" "\\override TupletBracket #'bracket-visibility = ##f")
	))
  
  (if (equal? current "\\override TupletBracket #'bracket-visibility = ##t ")
      (begin
	(d-DirectivePut-standalone-postfix "ToggleTupletBrackets" "\\override TupletBracket #'bracket-visibility = ##f ")
	(d-DirectivePut-standalone-graphic "ToggleTupletBrackets" "StartTupletBrackets"))
      (begin
	(d-DirectivePut-standalone-postfix "ToggleTupletBrackets" "\\override TupletBracket #'bracket-visibility = ##t ")
	(d-DirectivePut-standalone-graphic "ToggleTupletBrackets" "StopTupletBrackets")))
;;  (d-MoveCursorRight)
  (d-RefreshDisplay))
