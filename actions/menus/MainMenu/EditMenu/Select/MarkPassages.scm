;;;;MarkPassages
(define-once InsertDirectivesAroundSelection::start #f)
(define-once InsertDirectivesAroundSelection::end #f)
(if (not (or (d-HasSelection) (equal? "StartPassage" InsertDirectivesAroundSelection::start) (equal? "EndPassage" InsertDirectivesAroundSelection::end)))
	(begin 
		(StandAloneDirectiveProto (cons "EndPassage" "") #f "\nP)\ndenemo\n30" #f 40)
		(StandAloneDirectiveProto (cons "StartPassage" "") #f "\nP(\ndenemo\n30" #f 40)
		(d-SetMark)
		(d-CursorRight)))
(d-InsertDirectivesAroundSelection)
    