 ;;;;;;;; FindNextNoteHigher
 (d-MoveCursorRight)
(let loop ((lowest (d-GetCursorNoteAsMidi)) (current 0))
  (set! current (d-GetNoteAsMidi))
  (if (or (zero? current) (<= current lowest))
      (begin
	(if (d-MoveCursorRight)
	    (loop lowest current)
	    #t))))
      

  