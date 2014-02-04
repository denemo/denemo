;;;StaffMultiMeasureRests
(let ()
  (define (whole-measure-rest)
    (d-Directive-chord? "WholeMeasureRest"))

    ;;; find-block looks at the measures from the cursor onwards returning with the cursor on the first of a block of wmrs (#t) or at the end if none (#f)
  (define (find-block)
    (define position #f)
    (define count 0)
    (disp "find-block called")
    (let loop ()
      (if (whole-measure-rest)
        (begin
          (if (not position)
            (set! position (GetPosition)))
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

;;; actual code
  (d-PushPosition)
  (d-GoToBeginning)
  (while (and (not (whole-measure-rest)) (d-NextChord)))
  (while (and (not (whole-measure-rest)) (d-MoveToMeasureRight)))
  (let loop ()
    (if (find-block)
      (begin
    (d-MultiMeasureRests)
    (while (d-MoveToMeasureRight)
      (if (whole-measure-rest)
        (loop))))))
  (d-PopPosition))
