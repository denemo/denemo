(if (d-Directive-header "MovementTitle")
    (begin
        (if (d-PreviousMovement)
            (d-WarningDialog "Returning to the first Movement to set this,"))
        (while (d-PreviousMovement)
            (display "Seeking first movement"))
            (SetHeaderField "subtitle")
            (DenemoPrintAllHeaders)
            (d-RefreshDisplay))


(DenemoSetTitles "ScoreTitles" 'subtitle #f))
