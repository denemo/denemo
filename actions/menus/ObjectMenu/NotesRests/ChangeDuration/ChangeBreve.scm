;;;ChangeBreve
(d-SetBreve)
(if (Music?)
  (begin
	(if (Rest?) ; Test if its a note with a name or not, then its a rest
		(d-DirectivePut-chord-graphic "Duration" "rests_M1neomensural")  ;rest	
		(begin ;note
			(d-Change0) ; Change to a full note first to get rid of note-stems 
			(d-DirectivePut-chord-graphic "Duration" "

emmentaler")
		)	
	)
	(d-DirectivePut-chord-override "Duration" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
	(d-DirectivePut-chord-prefix "Duration" "\\breve ")
	(d-SetDurationInTicks (* 2 1536))
	(d-RefreshDisplay)
  )
  #f ; not music
)