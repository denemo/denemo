(define EducationGames::shiftup
  (lambda (n)
    (if (> n 0) (begin
		  (d-CursorUp)
		  (EducationGames::shiftup (- n 1))))))

(define EducationGames::shiftdown
  (lambda (n)
    (if (> n 0) (begin
		  (d-CursorDown)
		  (EducationGames::shiftdown (- n 1))))))

(define (EducationGames::PlaceAnswerStatus gfx)
  (begin
    (d-DirectivePut-note-minpixels "EducationGames::tick" 30)
    (d-DirectivePut-note-gx "EducationGames::tick" -15)
    (d-DirectivePut-note-gy "EducationGames::tick" 40)
    (d-DirectivePut-note-graphic "EducationGames::tick" gfx)))

;; set the random seed up using time of day
(let ((time (gettimeofday)))
  (set! *random-state*
          (seed->random-state (+ (car time)
	                         (cdr time)))))


