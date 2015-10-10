(if (d-Directive-header? "ScoreMeter")
    (begin

        (if (d-PreviousMovement)
            (d-WarningDialog "Returning to the first Movement to set this."))
        (while (d-PreviousMovement)
            (display "Seeking first movement"))
            (SetHeaderField "meter")
            (DenemoPrintAllHeaders)
            (d-RefreshDisplay))

    (DenemoSetTitles "ScoreTitles" 'meter #f))
