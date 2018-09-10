;;;ScoreComment
(let ((tag "ScoreComment"))
	(if (not (d-Directive-score? tag))
		(begin
			(d-DirectivePut-score-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_EDITOR))
			(d-DirectivePut-score-display tag (_ "Type any notes about the score you are working on here"))))
	(d-DisplayDirectiveTextEditor "score" tag))