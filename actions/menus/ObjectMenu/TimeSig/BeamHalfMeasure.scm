;;;;;;BeamHalfMeasure
(let ((tag "BeamHalfMeasure"))
    (if (d-Directive-timesig? tag)
        (begin
            (d-InfoDialog (_ "Beaming at half measure allowed"))
            (d-DirectiveDelete-timesig tag))
        (begin
            (d-DirectivePut-timesig-display tag (_ "Beaming breaks at half-measure"))
            (d-DirectivePut-timesig-postfix tag "
\\set Timing.beamHalfMeasure = ##f ")))
    (d-SetSaved #f))