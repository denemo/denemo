;; CheckForIncompleteMeasures
(d-PushPosition)
(d-MoveToBeginning)
(while (d-MoveToStaffUp))
(let loop ()
        (while (and (d-Directive-voice? "SubstituteMusic") (d-MoveToStaffDown)))
        (if (not  (d-Directive-voice? "SubstituteMusic"))
            (begin
            (while (and (AcceptableDurationMeasure?)  (d-MoveToMeasureRight))
        
              (if (and (AcceptableDurationMeasure?) (d-MoveToStaffDown))
              (begin 
                (while (and (d-Directive-voice? "SubstituteMusic") (d-MoveToStaffDown)))
                (d-MoveToBeginning)
                (if (not (d-Directive-voice? "SubstituteMusic"))
                    (loop))))))))
     
(if (or (d-Directive-voice? "SubstituteMusic") (AcceptableDurationMeasure?))
    (begin
      (d-PopPosition)
      (d-WarningDialog (_ "All measures appear complete.")))
    (begin 
      (d-PopPushPosition)
      (d-PopPosition)
      (d-WarningDialog (_ "This measure has the wrong duration."))))

