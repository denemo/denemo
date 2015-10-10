(if (d-Directive-header? "MovementPiece")
    (begin
        (SetHeaderField "piece")
        (DenemoPrintAllHeaders)
        (d-RefreshDisplay))
    (DenemoSetTitles "MovementTitles" 'piece #f))
