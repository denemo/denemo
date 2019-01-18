;;;ToggleNonPrinting
(d-UnsetMark) ;unset selection 
(d-SetMark) ;select object at cursor 
(d-SetNonprinting (not (d-GetNonprinting))) ;toggle non printing 
(d-UnsetMark) 
(d-MoveCursorRight)
(d-SetSaved #f) 