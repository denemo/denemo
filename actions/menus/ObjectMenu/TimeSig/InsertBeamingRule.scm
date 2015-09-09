;;;InsertBeamingRule
(let ((tag "InsertBeamingRule")(spec '()))
    (GoToMeasureBeginning)
    (if (not (Timesignature?))
        (begin
            (d-InsertTimeSig (d-GetPrevailingTimesig))
            (d-MoveCursorLeft)
            (d-HideTimesig)))
    (set! spec (assoc-set! spec 'delete d-DirectiveDelete-timesig))
    (set! spec (assoc-set! spec 'get d-DirectiveGet-timesig-data))
    (set! spec (assoc-set! spec 'put d-DirectivePut-timesig-data))
    (set! spec (assoc-set! spec 'proc d-DirectivePut-timesig-prefix))
    (set! spec (assoc-set! spec 'layout ""))
    (set! spec (assoc-set! spec 'tag tag))
    (d-SetBeamExceptions spec) 
    (d-DirectivePut-timesig-override tag DENEMO_OVERRIDE_GRAPHIC)
    (d-DirectivePut-timesig-display tag (_ "Beam")))
            
