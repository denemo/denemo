(if (d-Directive-scoreheader? "ScoreCopyright")
    (begin
        (SetScoreHeaderField "copyright" #f #f)
        (DenemoPrintAllHeaders)
        (d-SetSaved #f)
        (d-RefreshDisplay))
    
(DenemoSetTitles "ScoreTitles" 'copyright #f))
