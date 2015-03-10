;;CheckBeamsInMeasure

(define-once CheckScore::ignore 0)
(define CheckBeamsInMeasure::return #f)
(let ((tag "Beam"))
 (define (get-pos)
    (string-append "\nAt movement " (number->string (d-GetMovement)) ",  voice " (number->string (d-GetStaff)) ", measure " (number->string (d-GetMeasure)) "."))
  (define (hasBeamStart?)
    (equal? "[" (d-DirectiveGet-chord-postfix tag)))
  (define (hasBeamEnd?)
    (equal? "]" (d-DirectiveGet-chord-postfix tag)))
  (define start '())
    (while (d-PrevObjectInMeasure))
  (let loop ()
    ;(disp "loop" start "\n")
    (if (hasBeamStart?)
      (begin
        (if (< (d-GetNoteBaseDuration) 3)
          (begin
            (if (positive? CheckScore::ignore)
                (set! CheckScore::ignore (1- CheckScore::ignore))
                (set! CheckBeamsInMeasure::return (_ "Beam Start command on a note with no beam")))))
        (if (not (null? start))
            (begin
                (if (positive? CheckScore::ignore)
                    (set! CheckScore::ignore (1- CheckScore::ignore))
                    (set! CheckBeamsInMeasure::return  (string-append (_ "Second start Beam")))))
            (set! start (cons (GetPosition) start)))))
    (if (hasBeamEnd?)
      (begin
        (if (< (d-GetNoteBaseDuration) 3)
          (begin
            (if (positive? CheckScore::ignore)
                (set! CheckScore::ignore (1- CheckScore::ignore))
              (set! CheckBeamsInMeasure::return  (_ "Beam End command on a note with no beam"))))
        (if (null? start)
          (begin
                (if (positive? CheckScore::ignore)
                    (set! CheckScore::ignore (1- CheckScore::ignore))
                    (set! CheckBeamsInMeasure::return  (string-append (_ "End Beam with no start") (get-pos)))))
          (begin
            (set! start (cdr start))
            (if (d-NextObjectInMeasure)
            (loop)))))))
    (if (and (not CheckBeamsInMeasure::return) (d-NextObjectInMeasure))
      (loop)))
  (if (and (not CheckBeamsInMeasure::return) (not (null? start)))
    (begin
      (apply d-GoToPosition (car start))
      (set! CheckBeamsInMeasure::return  (string-append (_ "Start Beam with no end")  (get-pos)))))
  (if (not CheckBeamsInMeasure::params)
    (begin ;; interactive
        (if (not CheckBeamsInMeasure::return)
            (begin
                (set! CheckBeamsInMeasure::return (_ "No problem detected with beams in measure"))))
                (d-InfoDialog CheckBeamsInMeasure::return))))  
      
