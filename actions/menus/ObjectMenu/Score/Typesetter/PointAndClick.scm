;;;;;;;; PointAndClick
(if (d-Directive-score? "PointAndClick")
	(begin
		(d-DirectiveDelete-score "PointAndClick")
		(d-InfoDialog (_ "Point-and-click is now On.\nThis means that you will be able to click on the typeset score to move the cursor to a given note, or do editing etc.\nGenerated PDF files will be larger though.")))
	(begin
		(d-InfoDialog (_ "Point-and-click is now Off.\nThis means that you will be not able to click on the typeset score to move the cursor to a given note, or do editing etc.\nGenerated PDF files will be smaller. Use this for pdf export"))
		(d-DirectivePut-score-prefix "PointAndClick" "\n\\pointAndClickOff\n")))
(d-SetSaved #f)	
