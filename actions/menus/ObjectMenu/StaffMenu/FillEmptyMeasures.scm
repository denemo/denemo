;;;FillEmptyMeasures
(d-PushPosition)
(let loop ()
    (if (EmptyMeasure?)
        (begin
            (d-WholeMeasureRest 'nonprinting)
            (d-SetNonprinting)))
    (if (d-MoveToMeasureRight)
            (loop)))
(d-PopPosition)
