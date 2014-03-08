;;;DeleteEmptyMeasures    
(while (and (EmptyMeasure?) (> (d-GetMeasuresInStaff) 1))
    (d-DeleteMeasure))