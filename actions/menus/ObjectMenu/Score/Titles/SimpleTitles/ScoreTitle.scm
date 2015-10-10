(let ((tag "ScoreTitle") (title ScoreTitle::params))
    (if (d-Directive-scoreheader? tag)
        (begin
            (SetScoreHeaderField "title" title)
            (d-DirectivePut-header-postfix "SuppressTitleRepeats" "title = ##f\ninstrument = ##f\n")
            (DenemoPrintAllHeaders))   
        (DenemoSetTitles "ScoreTitles" 'title #f)))
