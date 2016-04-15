;;;ToggleTrill                   
	(let ((tag "ToggleTrill") (note (d-GetNoteAsMidi)) (lower #f)(upper #f) )
		(ChordAnnotation tag "\\trill"    ToggleTrill::params    LG-Trill)
			(if (d-Directive-chord? tag)
				(begin		
					(set! lower (string-append "(d-PlayMidiNote " (number->string note) "  255 0 100)"))
					(set! upper (string-append "(d-PlayMidiNote " (number->string (+ 2 note)) "  255 0 100)"))
					(eval-string upper)
					(d-OneShotTimer 100 lower)
					(d-OneShotTimer 200 upper)
					(d-OneShotTimer 300 lower)
					(d-OneShotTimer 400 upper)
					(d-OneShotTimer 500  (string-append "(d-PlayMidiNote " (number->string note) "  255 0 300)")))))
	