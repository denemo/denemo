;;;TremoloDownward
(let ((tag "TremoloDirection"))
	(if (d-Directive-chord? tag)
		(d-DirectiveDelete-chord tag)
		(begin
			(d-DirectivePut-chord-prefix tag "\\once \\override StemTremolo.slope =#-0.4 ")
			(d-DirectivePut-chord-display tag "\\")
			(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
			(d-SetSaved #f)))
	(d-RefreshDisplay))
