;;;ArbitraryTextAbove
(let ((thetext #f))
  (set! thetext (d-GetUserInput (_ "Give string") (_ "give your text") " "))
  (if thetext
      (begin
	(d-DirectivePut-chord-postfix "ArbitraryTextAbove" 
				      (string-append "^\\markup \\bold \"" thetext "\""))
	(d-DirectivePut-chord-display "ArbitraryTextAbove" thetext)
	(d-DirectivePut-chord-ty "ArbitraryTextAbove" -100)

	(d-RefreshDisplay))))
;;;End of scheme script