;;;;;;;;;;;;;;CriticalCommentaryIntro
(if (not (d-Directive-score?  "CriticalCommentaryIntro"))
	(begin
		(d-DirectivePut-score-override "CriticalCommentaryIntro" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_EDITOR))
		(d-DirectivePut-score-display "CriticalCommentaryIntro" "Critical Commentary\nKey: m = Movement, v = voice, b = bar\n")))
(d-DirectiveActivate-score  "CriticalCommentaryIntro" )
(d-SetSaved #f)