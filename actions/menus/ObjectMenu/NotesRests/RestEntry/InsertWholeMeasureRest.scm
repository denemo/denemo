;;;;InsertWholeMeasureRest
(if (or (None?)  (zero? (d-GetDurationInTicks)))
    (begin
        (d-WholeMeasureRest)
        (d-MoveCursorRight)
        (d-Set2))
    (begin  
        (if (d-MoveToMeasureRight)
             (if (or (None?)  (zero? (d-GetDurationInTicks)))
                (d-WholeMeasureRest)
                (d-MoveToMeasureLeft))
            (begin
                (d-AppendMeasureAllStaffs)  
                (d-MoveCursorRight)
                (d-WholeMeasureRest)
                (d-Set2)
                (d-MoveCursorRight)))))
