;;; ChordComment
(let ()
(define current (d-DirectiveGet-chord-display "ChordComment"))
(let script ((answer (d-GetUserInput (_ "Insert Comment") (_ "Comment this chord") (if current current ""))))
	(if (and answer (not (string=? answer "")))
		(begin
			(d-DirectivePut-chord-override "ChordComment" DENEMO_OVERRIDE_EDITOR)
			(d-DirectivePut-chord-display "ChordComment" answer))
		#f)))