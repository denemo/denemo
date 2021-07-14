;;;Upbeat
(let  ((upbeat "Upbeat") (ok #t)(params Upbeat::params))
  ; How many ticks are in a 100% filled measure?
  (define MaxTicks (* 1536 (GetPrevailingTimeSig #t) )) 
  (define EndTick #f)
  ;Upbeat is only for underful measures
  (define (warning)
   (d-InfoDialog (_ "Upbeat/Short Measure can only be used in an underfull, non-empty measure"))
   (set! ok #f)   
   #f)
 
 ; create upbeat directive and reduce display measure number by one
    (define (createUpbeat)
       (define remainingTicks (- MaxTicks EndTick))
       (define partialDuration (number->string (/ EndTick 6 )))
       (GoToMeasureBeginning)
       (StandAloneDirectiveProto (cons upbeat (string-append "\\partial 256*" partialDuration   " "))  #f #f upbeat)
       (d-SetDurationInTicks remainingTicks)
       (d-DirectivePut-standalone-override upbeat  DENEMO_OVERRIDE_DYNAMIC)
       (d-DirectivePut-standalone-graphic upbeat "\n\nemmentaler\n62")
       (d-DirectivePut-standalone-gx upbeat 20)
       (d-DirectivePut-standalone-gy upbeat 15)
       (d-LockDirective)
       (d-SetMeasureNumberOffset -1))
        
    (define (ComputeAndCreate)
            (begin  
                ; Save how many ticks are in this measure
            (GoToMeasureEnd)
            (set! EndTick (d-GetEndTick))
            ; Cond what to do, only create Upbeat if the measure is not full, else give warning.    
            (cond 
                ((not EndTick) (warning))
                ((zero? EndTick) (warning))
                ((not EndTick) (warning)) ; empty
                ((< EndTick MaxTicks) (createUpbeat)) ; underful
                ((= MaxTicks EndTick) (warning))  ; 100% filled
                ((< MaxTicks EndTick) (warning)) ; >100% filled
                (else  (warning)) ; ?
            )))
    (define (DeleteUpbeat)
        (if (d-Directive-standalone? upbeat)
            (begin
                (d-LockDirective #f) ;; unlock
                (d-DirectiveDelete-standalone upbeat)
                (d-SetMeasureNumberOffset 0))))


    (if (equal? params "delete")
        (DeleteUpbeat)
        (begin

              (GoToMeasureBeginning)
              
              (if Upbeat::params ;;; non-interactive call, this used to take the params as the tag to be used - is that useful?
                (DeleteUpbeat))
                
              (if (d-Directive-standalone? upbeat)
                    (let ( (choice (d-GetOption  (string-append (_ "Help") stop (_ "Re-calculate") stop (_ "Delete") stop (_ "Advanced") stop))))
                     (cond
                        ((boolean? choice)
                             (d-WarningDialog (_ "Operation cancelled")))
                         ((equal? choice (_ "Help"))                    
                            (d-InfoDialog (_ "This object fills up the duration of this measure, so that the notes in the measure form an upbeat. It needs to be renewed if you change the duration of the notes in the measure - use Re-calculate for this, or simply delete it and re-run the Upbeat command.")))
                        ((equal? choice (_ "Re-calculate"))
                            (DeleteUpbeat)
                            (ComputeAndCreate))     
                        ((equal? choice (_ "Delete"))
                            (DeleteUpbeat))
                            ((equal? choice (_ "Advanced"))
                            (if (not (d-DirectiveTextEdit-standalone upbeat))
                                (DeleteUpbeat)))))
                    ;;;if upbeat not already present
                    (ComputeAndCreate))
            (if (d-Directive-standalone? upbeat)
                (begin
                    (d-SetMark)
                    (d-Copy)
                    (d-UnsetMark)
                    (d-PushPosition)        
                    (while (d-MoveToStaffUp))
                    (if (d-Directive-clef? DenemoClickTrack)
                    	(d-MoveToStaffDown))
                    (let loop ()    
                        (DeleteUpbeat)
                        (d-Paste)
                        (d-MoveCursorLeft)
                        (if (d-Directive-standalone? upbeat)
                        	(d-LockDirective))
                        (d-SetMeasureNumberOffset -1)
                        (if (d-MoveToStaffDown)
                            (loop)))
                    (d-PopPosition)
                    (d-RefreshDisplay)
                    (if Upbeat::params 
                        (disp "Upbeat recalculated\n")
                        (begin
                            (if ok
                            (begin 
                                (GoToMeasureEnd)
                                (if  (not (d-MoveToMeasureRight))
                                    (d-AddMeasure)))))))))))  
        

