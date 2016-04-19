  ;;;ToggleTrill                   
	(let ((tag "ToggleTrill") (note (d-GetNoteAsMidi)) (lower #f)(upper #f) )
		(ChordAnnotation tag "\\trill"    ToggleTrill::params    LG-Trill)
			(if (d-Directive-chord? tag)
				(begin		
					(set! lower (string-append "(d-PlayMidiNote " (number->string note) "  255 0 60)"))
					(set! upper (string-append "(d-PlayMidiNote " (number->string (+ 2 note)) "  255 0 60)"))
					(eval-string upper)
					(d-OneShotTimer 60 lower)
					(d-OneShotTimer 120 upper)
					;(d-OneShotTimer 180 lower)
					;(d-OneShotTimer 240 upper)
					(d-OneShotTimer 180  (string-append "(d-PlayMidiNote " (number->string note) "  255 0 300)")))))
	