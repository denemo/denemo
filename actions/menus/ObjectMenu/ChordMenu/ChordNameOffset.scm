;;;ChordNameOffset
(let ((tag "ChordNameOffset"))
 (if (d-Directive-chord? tag)
 	(d-DirectiveDelete-chord  tag)
 	(begin
		(d-DirectivePut-chord-prefix tag "\\tweak ChordName.extra-offset  #'(-5 . -2.0) ")
		(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
		(d-DirectivePut-chord-display tag "<-->")))
(d-SetSaved #f))