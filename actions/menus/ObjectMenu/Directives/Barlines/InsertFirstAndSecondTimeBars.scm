;;;InsertFirstAndSecondTimeBars
(d-PushPosition)
(if (and (not (d-MoveToMeasureRight)) (Appending?))
    (begin
        (d-OpenFirstTimeBar)
        (d-InsertWholeMeasureRest)
        (d-RepeatEnd)
        (d-EndVolta)
        (d-OpenSecondTimeBar)
        (d-InsertWholeMeasureRest)
        (d-EndVolta)
        (d-RepeatStart))
    (d-WarningDialog (_ "Only for appending to music - use the individual first and second time bar commands to insert within music.")))
(d-PopPosition)
