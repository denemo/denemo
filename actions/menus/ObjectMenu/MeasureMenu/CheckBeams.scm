;;CheckBeamsInMeasure
(define CheckBeamsInMeasure::return #t)
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
    (disp "loop\n")
    (if (hasBeamStart?)
      (begin
        (if (< (d-GetNoteBaseDuration) 3)
          (begin
              (set! CheckBeamsInMeasure::return #f)
              (d-InfoDialog (_ "Beam Start command on a note with no beam"))))
        (if (not (null? start))
            (begin
              (set! CheckBeamsInMeasure::return #f)
              (d-InfoDialog (string-append (_ "Second start Beam"))))
            (set! start (cons (GetPosition) start)))))
    (if (hasBeamEnd?)
      (begin
        (if (< (d-GetNoteBaseDuration) 3)
          (begin
              (set! CheckBeamsInMeasure::return #f)
              (d-InfoDialog (_ "Beam End command on a note with no beam"))))
        (if (null? start)
          (begin
            (set! CheckBeamsInMeasure::return #f)
            (d-InfoDialog (string-append (_ "End Beam with no start") (get-pos))))
          (begin
            (set! start (cdr start))
            (if (d-NextObjectInMeasure)
            (loop))))))
    (if (and CheckBeamsInMeasure::return (d-NextObjectInMeasure))
      (loop)))
  (if (and CheckBeamsInMeasure::return (not (null? start)))
    (begin
      (apply d-GoToPosition (car start))
      (set! CheckBeamsInMeasure::return #f)
      (d-InfoDialog (string-append (_ "Start Beam with no end")  (get-pos))))))
