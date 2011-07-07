(if (and (= 1 (d-GetMovement)) (not (d-Directive-header? "ScoreTitle")))
(SetHeaderField "title" "Untitled"))
(d-DirectiveDelete-scoreheader "ScoreTagline")
(define (InitializeTypesetting)
  (d-CheckScore)
  (if (not CheckScore::return)
    (begin
	(d-InfoDialog "Score Check: Error in this measure - fix before printing")
	(exit))))

(d-LimitInterSystemSpace 1.2)
(d-SetSaved #t)
