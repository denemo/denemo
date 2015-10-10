(if (d-Directive-scoreheader? "ScoreInstrument")
    (begin
        (SetScoreHeaderField "instrument" #f #f)
        (DenemoPrintAllHeaders)
        (d-SetSaved #f)
        (d-RefreshDisplay))


(DenemoSetTitles "ScoreTitles" 'instrument #f))
