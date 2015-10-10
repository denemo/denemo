(if (d-Directive-header? "ScorePoet")
    (begin
        (if (d-PreviousMovement)
            (d-WarningDialog "Returning to the first Movement to set this."))
        (while (d-PreviousMovement)
            (display "Seeking first movement"))
            (SetHeaderField "poet")
            (DenemoPrintAllHeaders)
            (d-RefreshDisplay))
        

(DenemoSetTitles "ScoreTitles" 'poet #f))
