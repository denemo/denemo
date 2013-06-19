;;;ChangeLonga
(d-SetLonga)
(if (Music?)
  (begin ; its a chord, means note or rest!
	(if (Rest?) ; Test if its a note with a name or not, then its a rest
		(d-DirectivePut-chord-graphic "Duration" "

emmentaler") ;rest
		(begin ;note
			(d-Change0) ; Change to a full note first to get rid of note-stems 
			(d-DirectivePut-chord-graphic "Duration" "

emmentaler")	
		)	
	)
	(d-DirectivePut-chord-prefix "Duration" "\\longa ")
	(d-DirectivePut-chord-override "Duration" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
	(d-SetDurationInTicks (* 4 1536))
	(d-RefreshDisplay)
  )
  #f ; not a chord
)