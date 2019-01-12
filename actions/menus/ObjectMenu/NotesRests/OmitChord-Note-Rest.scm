(let ((tag "OmitChord-Note-Rest"))
(if (Music?)
	(begin
		(if (d-Directive-chord? tag)
			(d-DirectiveDelete-chord tag)
			(begin
				(d-DirectivePut-chord-prefix tag "%{ Note/chord/rest Hidden")
				(d-DirectivePut-chord-postfix tag " %}\n")
				(d-DirectivePut-chord-graphic tag "\n⋂\nDenemo\n24")
				(d-DirectivePut-chord-gy tag -60)
				(d-DirectivePut-chord-override tag 16)
				(d-DirectiveOnlyForLayout)
				(d-SetSaved #f))))))
