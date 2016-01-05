;;InsertGraceNoteHints FIX FOR EMPTY MEASURES
(let ((last-object 'none) (notice #f))
  (define (GetStartTick)
    (define tick (d-GetStartTick))
    (if tick tick 0))
   (define (clean-measure)
    (let loop ()
      (if (and (Rest?) (d-IsGrace))
        (begin
          (d-DeleteObject)
          (loop)))
      (if (d-NextObjectInMeasure)
          (loop))))

  (define (get-grace)
    (define str "(d-InsertBlankWholeNote)(d-MoveCursorLeft)(d-ToggleGrace)")
    (let ((duration (d-GetNoteBaseDuration)) )
     (set! str (string-append str "(d-Change" (number->string duration) ")(d-MoveCursorRight)")))
     (let loop ()
        (if (d-NextChordInMeasure)
          (if (d-IsGrace)
            (begin
              (set! str (string-append str "(d-InsertBlankWholeNote)(d-MoveCursorLeft)(d-ToggleGrace)" "(d-Change" (number->string (d-GetNoteBaseDuration)) ")(d-MoveCursorRight)"))
              (loop)))))
      str)
      
  (define (no-grace-at-tick start-tick)
    (define ret #t)
    (d-PushPosition)
    (let loop ()
        (if (= start-tick (GetStartTick))
          (if (d-IsGrace)
            (set! ret #f)
            (if (d-NextObjectInMeasure)
              (loop)))))
    (d-PopPosition)
    (if (not (Music?))
      (d-NextObjectInMeasure))
  ret)
          
  (define (ensure-grace start-tick grace)
    (let loop () 
      (if (and (> start-tick (GetStartTick)) (d-NextObjectInMeasure))
        (loop)
        (begin
          (if (= (GetStartTick) start-tick)
            (if (no-grace-at-tick start-tick)
                  (eval-string grace)))))))


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
    (if (and (MeasureComplete?) (dangerous-grace?))
          (let ((start-tick (GetStartTick)) (grace (get-grace)))
            (set! notice (_ "Grace note hints installed"))
            (d-PushPosition)
            (while (MoveUpStaffOrVoice))
            (while (d-PrevObjectInMeasure)) ;;if it doesn't go up a staff we may not be at the start.
            (ensure-grace start-tick grace)
            (let loop ()
              (if (MoveDownStaffOrVoice)
                (begin
                    (if (MeasureComplete?)
                        (ensure-grace start-tick grace))
                  (loop))))
            (d-PopPosition))))


  (define (action-staff action)
    (d-MoveToBeginning)
    (action)
    (while (d-MoveToMeasureRight)
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
  (if notice
    (TimedNotice notice))
  (d-PopPosition))
  ;;;;;;;;;;;;;;;;;;;;;;
