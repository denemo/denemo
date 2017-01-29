(let ((tag "ConnectArpeggios"))
(if (d-Directive-score? tag)
	(d-DirectiveDelete-score tag)
	(begin
		(d-DirectivePut-score-prefix tag "\\layout {
		    \\context {
		      \\Score
		      \\consists \"Span_arpeggio_engraver\"
		     connectArpeggios = ##t    }
		  }")
		(d-DirectivePut-score-display tag (_ "Connect Arpeggios"))))
(d-SetSaved #f))
