;(display "Creating duration edit buttons")
 (let loop ((count 0))
   (define dur (number->string count))
(CreateButton (string-append "ButtonChange" dur) (string-append "~ " (vector-ref MusicalSymbols-notes count)))
(d-SetDirectiveTagActionScript  (string-append "ButtonChange" dur) (string-append "(d-Change" dur ")"))
(if (< count 7)
    (loop (+ count 1))))

(CreateButton "ButtonAddDot" "." )
(d-SetDirectiveTagActionScript  "ButtonAddDot"  "(d-AddDot)")
(CreateButton  "ButtonRemoveDot" "~.")
(d-SetDirectiveTagActionScript  "ButtonRemoveDot"  "(d-RemoveDot)")
(CreateButton  "ButtonSharpen" MusicalSymbols-sharp)
(d-SetDirectiveTagActionScript  "ButtonSharpen" "(d-Sharpen)")
(CreateButton  "ButtonFlatten" MusicalSymbols-flat)
(d-SetDirectiveTagActionScript  "ButtonFlatten" "(d-Flatten)")

(CreateButton  "ButtonStartSlur" "()")
(d-SetDirectiveTagActionScript  "ButtonStartSlur" "(d-SlurTwo)")

(CreateButton  "ButtonExtendSlur" "(")
(d-SetDirectiveTagActionScript  "ButtonExtendSlur" "(d-ExtendSlur)")

(CreateButton  "ButtonReduceSlur" ")")
(d-SetDirectiveTagActionScript  "ButtonReduceSlur" "(d-ReduceSlur)")

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

(d-PointAndClick)
(d-SetSaved #t)
