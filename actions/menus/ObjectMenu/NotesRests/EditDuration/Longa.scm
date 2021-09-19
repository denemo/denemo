;;Longa
(let ((spillover (d-GetBooleanPref "spillover")) )
	(d-SetPrefs "<spillover>0</spillover>")
	(d-0)
	(d-MoveCursorLeft)
	(d-ChangeLonga);
	(if (and (defined? 'Snippet::Longa) (> Snippet::Longa 0))
		(d-SelectSnippet Snippet::Longa)
		(d-CreateSnippetFromObject "Longa"))
	(d-MoveCursorRight)
	(if spillover 
		(d-SetPrefs "<spillover>1</spillover>")))

