 ;;;;;;;;BeamingOn
(let ((tag  "BeamingOn"))
 (if (d-Directive-standalone? tag)
    (EditForStandaloneToggle tag)   
    (begin
        (if (d-MoveCursorLeft)
            (if (d-Directive-standalone? tag)
              (d-DirectiveDelete-standalone tag)
              (d-MoveCursorRight)))
        (StandAloneDirectiveProto (cons tag "\\autoBeamOn") #f #f (_ "Normal Beaming"))
        (d-MoveCursorRight)
        (d-RefreshDisplay)
        (d-SetSaved #f))))
        
