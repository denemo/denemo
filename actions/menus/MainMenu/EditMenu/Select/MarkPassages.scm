;;;;MarkPassages
(define-once InsertDirectivesAroundSelection::start #f)
(define-once InsertDirectivesAroundSelection::end #f)
(if (or (not (d-HasSelection)) (not (equal? "StartPassage" InsertDirectivesAroundSelection::start)) (not (equal? "EndPassage" InsertDirectivesAroundSelection::end)))
	(begin 
		(StandAloneDirectiveProto (cons "EndPassage" "") #f "\nP)\ndenemo\n30" #f 40)
		(StandAloneDirectiveProto (cons "StartPassage" "") #f "\nP(\ndenemo\n30" #f 40)
		(d-SetMark)
		(d-CursorRight)))
(d-InsertDirectivesAroundSelection)
    