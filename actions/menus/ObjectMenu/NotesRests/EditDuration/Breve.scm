;;Breve
(let ((spillover (d-GetBooleanPref "spillover")) )
	(d-SetPrefs "<spillover>0</spillover>")
	(d-0)
	(d-MoveCursorLeft)
	(d-ChangeBreve);
	(if (and (defined? 'Snippet::Breve) (> Snippet::Breve 0))
		(d-SelectSnippet Snippet::Breve)
		(d-CreateSnippetFromObject "Breve"))
	(d-MoveCursorRight)
	(if spillover 
		(d-SetPrefs "<spillover>1</spillover>")))

