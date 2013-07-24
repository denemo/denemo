;;AvoidSlurAccidentalCollision
;;;http://code.google.com/p/lilypond/issues/detail?id=796
(let ((tag "AvoidSlurAccidentalCollision"))
    (if (d-Directive-chord? tag)
 	(begin
 		(d-DirectiveDelete-chord  tag)
 		(d-InfoDialog (_ "Slur/Accidental avoidance removed")))
 	(begin
		(d-DirectivePut-chord-prefix tag "\\once \\override Slur #'details #'edge-attraction-factor = #1 ")
		(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
		(d-DirectivePut-chord-display tag "(X")))
(d-RefreshDisplay)
(d-SetSaved #f))
