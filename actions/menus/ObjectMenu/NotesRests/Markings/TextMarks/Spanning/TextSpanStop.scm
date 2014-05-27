;;;TextSpanStop
(if (Music?)
	(let ((tag "TextSpanStop")(text "...|"))
	(if (d-Directive-chord? tag)
		(d-DirectiveDelete-chord tag)
		(begin
					(d-DirectivePut-chord-postfix tag "\\stopTextSpan")
					(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
					(d-DirectivePut-chord-display tag  text)))
	(d-SetSaved #f)))