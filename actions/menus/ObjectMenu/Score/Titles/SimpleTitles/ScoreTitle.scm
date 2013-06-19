;;;ScoreTitle
(let ((tag "ScoreTitle") (title ScoreTitle::params))
	(SetScoreHeaderField "title" title)
  (d-DirectivePut-header-postfix "SuppressTitleRepeats" "title = ##f\ninstrument = ##f\n")
	(DenemoPrintAllHeaders))		
		