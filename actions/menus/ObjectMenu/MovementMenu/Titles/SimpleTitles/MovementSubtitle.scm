(if (d-Directive-header? "MovementSubtitle")
    (begin
        (SetHeaderField "subsubtitle")
        (DenemoPrintAllHeaders)
        (d-RefreshDisplay))
    (DenemoSetTitles "MovementTitles" 'subtitle #f))
