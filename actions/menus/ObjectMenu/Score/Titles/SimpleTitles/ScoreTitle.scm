;;;ScoreTitle
(let ((tag "ScoreTitle") (title ScoreTitle::params))
  (if (equal? title "edit")
    (set! title #f))
  (SetScoreHeaderField "title" title)
  (d-DirectivePut-header-postfix "SuppressTitleRepeats" "title = ##f\ninstrument = ##f\n")
    (DenemoPrintAllHeaders))        
        
