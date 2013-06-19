;;;tuplet numbering toggle
(let ((current ""))
  (set! current (d-DirectiveGet-standalone-postfix "ToggleTupletNumbering"))
  (if (boolean? current)
      (begin
	(d-DirectivePut-standalone-minpixels "ToggleTupletNumbering" 20)
	(d-MoveCursorLeft)
	(set! current  "\\override TupletNumber #'transparent = ##f ")
	(d-DirectivePut-standalone-postfix "ToggleTupletNumbering" "\\override TupletNumber #'transparent = ##f ")
	))
  
  (if (equal? current "\\override TupletNumber #'transparent = ##t ")
      (begin
	(d-DirectivePut-standalone-postfix "ToggleTupletNumbering" "\\override TupletNumber #'transparent = ##f ")
	(d-DirectivePut-standalone-gx "ToggleTupletNumbering" 3)
	(d-DirectivePut-standalone-gy "ToggleTupletNumbering" -40)
        (d-DirectivePut-standalone-graphic "ToggleTupletNumbering" "StartTupletNumbering"))
      (begin
	(d-DirectivePut-standalone-postfix "ToggleTupletNumbering" "\\override TupletNumber #'transparent = ##t ")
	(d-DirectivePut-standalone-gx "ToggleTupletNumbering" 3)
	(d-DirectivePut-standalone-gy "ToggleTupletNumbering" -40)
	(d-DirectivePut-standalone-graphic "ToggleTupletNumbering" "StopTupletNumbering")))
;;  (d-MoveCursorRight)
  (d-RefreshDisplay))
