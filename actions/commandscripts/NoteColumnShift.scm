;;NoteColumnShift
(let* ( (tag "NoteColumnShift") (val (d-DirectiveGet-chord-display tag)))
(if (not val)
  (set! val "1.2"))
(set! val (d-GetUserInput (_ "Horizontal Shift") (_ "Give amount (staff spaces)") val))
(if val 
	(begin
		(if (string-null? val)
			(d-DirectiveDelete-chord tag)
			(begin
				(d-DirectivePut-chord-prefix tag (string-append "\\once \\override NoteColumn #'force-hshift = #" val " "))
				(d-DirectivePut-chord-display tag val)
				(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)))
		(d-SetSaved #f))))
