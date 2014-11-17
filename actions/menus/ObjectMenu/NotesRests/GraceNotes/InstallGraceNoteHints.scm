;;InsertGraceNoteHints
(let ((last-object 'none))
   (define (clean-measure)
    (let loop ()
      (if (and (Rest?) (d-IsGrace))
        (begin
          (d-DeleteObject)
          (loop)))
      (if (d-NextObjectInMeasure)
          (loop))))

  (define (get-grace)
    (define str "(d-InsertBlankWholeNote)(d-ToggleGrace)")
    (let ((duration (d-GetNoteBaseDuration)) )
     (set! str (string-append str "(d-Change" (number->string duration) ")(d-MoveCursorRight)")))
     (let loop ()
        (if (d-NextChordInMeasure)
          (if (d-IsGrace)
            (begin
              (set! str (string-append str "(d-InsertBlankWholeNote)(d-ToggleGrace)" "(d-Change" (number->string (d-GetNoteBaseDuration)) ")(d-MoveCursorRight)"))
              (loop)))))
      str)
      
  (define (no-grace-at-tick start-tick)
    (define ret #t)
    (d-PushPosition)
    (let loop ()
        (if (= start-tick (d-GetStartTick))
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
      (if (and (> start-tick (d-GetStartTick)) (d-NextObjectInMeasure))
        (loop)
        (begin
          (if (= (d-GetStartTick) start-tick)
            (if (no-grace-at-tick start-tick)
                  (eval-string grace)))))))


  (define (dangerous-grace?) 
    (let loop () (disp "have " (d-IsGrace) " " (d-GetMeasureNumber)  "\n")
      (if (not (and (d-IsGrace) (not (d-GetNonprinting)) last-object))
        (begin  
          (set! last-object (not (Music?)))
          (if (d-NextObjectInMeasure)
            (loop)))))
    (d-IsGrace))
          

  (define (fix-measure)
    (set! last-object 'beginning)
    (if (dangerous-grace?)
      (let ((start-tick (d-GetStartTick)) (grace (get-grace)))
        
        (d-PushPosition)
        (while (MoveUpStaffOrVoice))
        (while (d-PrevObjectInMeasure)) ;;if it doesn't go up a staff we may not be at the start.
        (ensure-grace start-tick grace)
        (let loop ()
          (if (MoveDownStaffOrVoice)
            (begin
              (ensure-grace start-tick grace)
              (loop))))
        (d-PopPosition))))


  (define (action-staff action)
    (d-MoveToBeginning)
    (action)
    (while (d-MoveToMeasureRight)
       (action)))
      
  (define (action-movement action)
    (action-staff action)(disp "Did " (d-GetStaff) "\n")
    (while (MoveDownStaffOrVoice)
      (action-staff action)))
    

  ;;;actual procedure follows
  (d-PushPosition)
  (while (MoveUpStaffOrVoice))
  (action-movement clean-measure)
  (while (MoveUpStaffOrVoice))
  (action-movement fix-measure)
  (d-PopPosition))