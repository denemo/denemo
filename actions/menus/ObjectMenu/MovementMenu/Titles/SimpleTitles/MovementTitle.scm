(if (d-Directive-header? "MovementTitle")
    (begin
        (SetHeaderField "subtitle")
        (d-DirectivePut-header-postfix "SuppressTitleRepeats" "title = ##f\ninstrument = ##f\n")
        (DenemoPrintAllHeaders)
        (d-SetSaved #f))
    (DenemoSetTitles "MovementTitles" 'title #f))
