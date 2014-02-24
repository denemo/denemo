;;;;InsertWholeMeasureRest
(if (or (None?)  (zero? (d-GetDurationInTicks)))
    (begin
        (DenemoWholeMeasureRestCommand)
        (d-MoveCursorRight)
        (d-Set2))
    (begin  
        (if (d-MoveToMeasureRight)
             (if (or (None?)  (zero? (d-GetDurationInTicks)))
                (DenemoWholeMeasureRestCommand)
                (begin
                	(d-MoveToMeasureLeft)
                	(if (d-Directive-chord? DenemoWholeMeasureRestTag)
                		(DenemoWholeMeasureRestCommand))))
            (begin
                (d-AppendMeasureAllStaffs)  
                (d-MoveCursorRight)
                (DenemoWholeMeasureRestCommand)
                (d-Set2)
                (d-MoveCursorRight)))))
