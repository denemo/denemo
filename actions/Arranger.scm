(if (and (= 1 (d-GetMovement)) (not (d-Directive-header? "ScoreTitle")))
(SetHeaderField "title" "Untitled"))
(d-DirectiveDelete-scoreheader "ScoreTagline")

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
(d-LimitInterSystemSpace 1.2)
(d-SetSaved #t)
