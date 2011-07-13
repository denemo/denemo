;(display "Creating duration edit buttons")
 (let loop ((count 0))
   (define dur (number->string count))
(CreateButton (string-append "ButtonChange" dur) (string-append "~<span  size=\"10000\" face=\"Denemo\">" dur "</span>"))
(d-SetDirectiveTagActionScript  (string-append "ButtonChange" dur) (string-append "(d-Change" dur ")"))
(if (< count 7)
    (loop (+ count 1))))

(CreateButton "ButtonAddDot" "." )
(d-SetDirectiveTagActionScript  "ButtonAddDot"  "(d-AddDot)")
(CreateButton  "ButtonRemoveDot" "~.")
(d-SetDirectiveTagActionScript  "ButtonRemoveDot"  "(d-RemoveDot)")
(CreateButton  "ButtonSharpen" "#")
(d-SetDirectiveTagActionScript  "ButtonSharpen" "(d-Sharpen)")
(CreateButton  "ButtonFlatten" "b")
(d-SetDirectiveTagActionScript  "ButtonFlatten" "(d-Flatten)")
(CreateButton  "ButtonFlatten" "b")
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
  (d-CheckScore)
  (if (not CheckScore::return)
    (let ((ok 
	(d-GetUserInput "Score Check: Error in this measure" "Try to print anyway?" "n")))
	(disp "note ok is " ok "\n")
	(if (equal? ok "n")
	(begin
		(disp "we have ok = n\n")
		(exit))))))


(d-SetSaved #t)
