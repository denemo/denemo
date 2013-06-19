;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Edit score prolog
(let ((lily "") (current ""))
   (set! current (d-ScoreProperties "query=lilypond"))
   (set! lily (d-GetUserInput (_ "Edit Score Prolog") 
			       (_ "Edit LilyPond to apply to whole score") 
	                       current))

   (d-ScoreProperties (string-append "lilypond=" lily "\0")))
;;;;;;;;;;;;;;;;;