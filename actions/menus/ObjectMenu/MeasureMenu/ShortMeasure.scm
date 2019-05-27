;;;ShortMeasure
(let  ((upbeat "ShortMeasure") (ok #t))
  ; How many ticks are in a 100% filled measure?
  (define MaxTicks (* 1536 (GetPrevailingTimeSig #t) )) 
  (define EndTick #f)
  ;Upbeat is only for underful measures
  (define (warning)
   (d-InfoDialog (_ "Upbeat/Short Measure can only be used in an underfull, non-empty measure"))
   (set! ok #f)   
   #f)
 
 ; Define Upbeat-Directive Subprogram
 (define (createUpbeat)
   (define remainingTicks (- MaxTicks EndTick))
   (define partialDuration (number->string (/ EndTick 6 )))
   (GoToMeasureBeginning)
   (StandAloneDirectiveProto (cons upbeat (string-append "\\set Timing.measurePosition = #(ly:make-moment -" (number->string (/ (GetMeasureTicks) 1536))    ") "))  #f #f upbeat)
   (d-SetDurationInTicks remainingTicks)
   (d-DirectivePut-standalone-override upbeat (logior DENEMO_OVERRIDE_DURATION  DENEMO_OVERRIDE_DYNAMIC))
   (d-DirectivePut-standalone-graphic upbeat "\n\nemmentaler\n36")
   (d-DirectivePut-standalone-gx upbeat 20)
    (d-DirectivePut-standalone-gy upbeat 15))
    
(define (ComputeAndCreate)
        (begin  
            ; Save how many ticks are in this measure
        (GoToMeasureEnd)
        (set! EndTick (d-GetEndTick))
        ; Cond what to do, only create Upbeat if the measure is not full, else give warning.    
        (cond 
            ((not EndTick) (warning)) ; empty
            ((< EndTick MaxTicks) (createUpbeat)) ; underful
            ((= MaxTicks EndTick) (warning))  ; 100% filled
            ((< MaxTicks EndTick) (warning)) ; >100% filled
            (else  (warning)) ; ?
        )))
    
 

  (GoToMeasureBeginning)
  
  (if Upbeat::params ;;; non-interactive call, this used to take the params as the tag to be used - is that useful?
    (d-DirectiveDelete-standalone upbeat))
    
  (if (d-DirectiveGet-standalone-display upbeat); if upbeat is present
        (let ( (choice (d-GetOption  (string-append (_ "Help") stop (_ "Re-calculate") stop (_ "Delete") stop (_ "Advanced") stop))))
         (cond
                 ((boolean? choice)
                     (d-WarningDialog (_ "Operation cancelled")))
                 ((equal? choice (_ "Help"))                    
                    (d-InfoDialog (_ "This object fills up the duration of this measure, without taking space in the typeset score. Use if for partial measures in first and second repeats. It needs to be renewed if you change the duration of the notes in the partial measure - use Re-calculate for this, or simply delete it and re-run the Short command.")))
                ((equal? choice (_ "Re-calculate"))
                    (d-DirectiveDelete-standalone upbeat)
                    (ComputeAndCreate))     
             ((equal? choice (_ "Delete"))
                (d-DeleteObject))
            ((equal? choice (_ "Advanced"))
                (if (not (d-DirectiveTextEdit-standalone upbeat))
                    (d-DirectiveDelete-standalone upbeat))
                )))
        ;;;if upbeat not already present
        (ComputeAndCreate))
(d-PushClipboard)
(d-SetMark)
(d-Copy)
(d-UnsetMark)
(d-PushPosition)        
(while (d-MoveToStaffUp))
(let loop ()    
    (if (d-Directive-standalone? upbeat)
        (d-DirectiveDelete-standalone upbeat))
    (d-Paste)
    (if (d-MoveToStaffDown)
        (loop)))
(d-PopPosition)
(d-PopClipboard)
 (d-RefreshDisplay)
 (if Upbeat::params 
    (disp "Short recalculated\n")
    (begin
        (if ok
        (begin 
            (GoToMeasureEnd)
            (if  (not (d-MoveToMeasureRight))
                (d-AddMeasure)))))))    
        

