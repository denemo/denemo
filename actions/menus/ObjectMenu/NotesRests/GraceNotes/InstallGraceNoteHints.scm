;;InsertGraceNoteHints
(let ((last-object 'none))
  (define (clean-measure)
    (let loop ()
      (if (and (Rest?) (d-IsGrace))
        (d-DeleteObject))
      (if (d-NextObjectInMeasure)
          (loop))))
          
  (define (get-grace)
    (d-SetMark) ;;;FIXME single grace notes only
    (d-Copy))
    
  (define (insert-grace-rest)
          (d-PrevObjectInMeasure)
          (d-Paste)
          (d-MoveCursorLeft) ;;;FIXME single grace notes only
          (d-StagedDelete)
          (d-SetNonprinting))
          
  (define (ensure-grace start-tick) 
    (let loop ()
      (if (and (> start-tick (d-GetStartTick)) (d-NextObjectInMeasure))
        (loop)
        (let inner-loop () 
          (if (= (d-GetStartTick) start-tick)
            (if (not (d-IsGrace))
              (if (d-NextObjectInMeasure)
                (inner-loop)))
            (if (> (d-GetStartTick) start-tick)
              (insert-grace-rest)))))))


  (define (dangerous-grace?) 
    (let loop ()
      (if (not (and (d-IsGrace) (not (d-GetNonprinting)) last-object))
        (begin  
          (set! last-object (not (Music?)))
          (if (d-NextObjectInMeasure)
            (loop)))))
    (d-IsGrace))
          

  (define (fix-measure)
    (set! last-object 'beginning)
    (if (dangerous-grace?)
      (let ((start-tick (d-GetStartTick)))
        (get-grace)
        (d-PushPosition)
        (while (MoveUpStaffOrVoice))
        (ensure-grace start-tick)
        (let loop ()
          (if (MoveDownStaffOrVoice)
            (begin
              (ensure-grace start-tick)
              (loop))))
        (d-PopPosition))))


  (define (action-staff action)
    (d-MoveToBeginning)
    (action)
    (if (d-MoveToMeasureRight)
      (action)))
      
  (define (action-movement action)
    (action-staff action)
    (while (MoveDownStaffOrVoice)
      (action-staff action)))
    

  ;;;actual procedure follows
  (d-PushPosition)
  (while (MoveUpStaffOrVoice))
  (action-movement clean-measure)
  (while (MoveUpStaffOrVoice))
  (action-movement fix-measure)
  (d-PopPosition))