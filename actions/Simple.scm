(d-MouseInsertion)
(if (and (= 1 (d-GetMovement)) (not (d-Directive-header? "ScoreTitle")))
(SetHeaderField "title" "Untitled"))

(d-LimitInterSystemSpace 1.2)
(define (InitializeTypesetting)
  (d-PushPosition)
  (d-CheckScore)
  (if (not CheckScore::return)
    (let ((ok 
	(d-GetUserInput "Score Check: Error in this measure" "Try to print anyway?" "n")))
	(if (equal? ok "n")
	  (begin
		(d-PopPushPosition)
		(d-PopPosition)
		(exit))))
	    (d-PopPosition)))
(d-SetSaved #t)
(display "Simple Profile\n")
