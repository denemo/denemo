(if (d-Directive-header? "MovementComposer")
    (begin
        (SetHeaderField "composer")
        (DenemoPrintAllHeaders)
        (d-SetSaved #f))
    (DenemoSetTitles "MovementTitles" 'composer #f))
