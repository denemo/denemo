;;;FillEmptyMeasures
(d-PushPosition)
(let loop ()
    (if (EmptyMeasure?)
        (begin
            (DenemoWholeMeasureRestCommand 'nonprinting)
            (d-SetNonprinting)))
    (if (d-MoveToMeasureRight)
            (loop)))
(d-PopPosition)
