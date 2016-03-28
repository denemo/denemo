;;;StaffMultiMeasureRests
(let ((end-position #f)(count 0)(position #f)(WMRtag "WholeMeasureRest"))
  (define (whole-measure-rest)
    (and (not (d-GetNonprinting))(d-Directive-chord? DenemoWholeMeasureRestTag)  (not (d-DirectiveGetNthTag-chord 1))  ))

    ;;; find-block looks at the measures from the cursor onwards returning with the cursor on the first of a block of wmrs (#t) or at the end if none (#f)
  (define (find-block)
    (set! position #f)
    (set! end-position #f)
    (set! count 0)
    (let loop ()
      (if (whole-measure-rest)
        (begin
          (if position
            (set! end-position (GetPosition))
            (begin
                (d-SetMark)
                (set! position (GetPosition))))
          (set! count (+ count 1))
          (if (d-MoveToMeasureRight)
            (loop)))
          (begin ;; not on a whole measure rest
            (if (zero? count)
              (if (d-MoveToMeasureRight)
                (loop)))
            (if (< count 2) ;;; do not MM less than 2
              (begin
                (set! count 0)
                (set! position #f)
                (if (d-MoveToMeasureRight)
                (loop))))))
      (if position
          (apply d-GoToPosition position)
          #f)))
    (define (enough-empty-measures? num)
            (if (or (EmptyMeasure?) (d-Directive-chord? WMRtag))
                (if (positive? num)
                    (begin
                        (if (d-Directive-chord? WMRtag) (d-DeleteObject));delete any whole measure rest
                        (if (EmptyMeasure?)
                            (d-MoveToMeasureRight))
                        (enough-empty-measures? (1- num)))
                    (EmptyMeasure?))
                #f))      
    (define (paste-into-voices) ;;; if voice(s) below has/have empty measures
        (while (d-MoveToVoiceDown)
             (apply d-GoToPosition position)
             (if (enough-empty-measures? (1- count))
                (begin
                    (apply d-GoToPosition position)
                    (d-Paste)))))
    (define (paste-into-dynamics) ;;; if Dynamics staff below has empty measures
        (if (and (d-MoveToStaffDown) (d-Directive-staff? "DynamicsStaff"))
            (begin
             (apply d-GoToPosition position)
             (if (enough-empty-measures? (1- count))
                (begin
                    (apply d-GoToPosition position)
                    (d-Paste))))))

;;; actual code
  (while (d-MoveToVoiceUp))
  (d-PushPosition)
  (d-GoToBeginning)
  (while (and (not (whole-measure-rest)) (d-NextChord)))
  (while (and (not (whole-measure-rest)) (d-MoveToMeasureRight)))
  (let loop ()
    (if (find-block)
      (begin
        (d-MultiMeasureRests)
        ;;;set staff element of position and end-position to #f
        (list-set! position 0 #f)
        (list-set! end-position 0 #f)
        (list-set! position 1 #f)
        (list-set! end-position 1 #f)
        (list-set! position 3 1);;first position, either first object or Appending position
        (list-set! end-position 3 1)
        (apply d-GoToPosition position)
        (d-SetMark)
        (apply d-GoToPosition end-position)
        (d-SetPoint)
        (d-Copy)
        (apply d-GoToPosition position)
        (d-PushPosition)
        (paste-into-voices)
        (paste-into-dynamics)
        (d-PopPosition)
        (apply d-GoToPosition end-position)
        (while (d-MoveToMeasureRight)
          (while (and (not (whole-measure-rest)) (d-NextChord)))
          (if (whole-measure-rest)
            (loop))))))
  (d-PopPosition))
