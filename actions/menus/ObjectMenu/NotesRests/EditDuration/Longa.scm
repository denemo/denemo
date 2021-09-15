;;Longa
(let ((spillover (d-GetBooleanPref "spillover")) (appending (or (None?) (Appending?))))
(d-SetPrefs "<spillover>0</spillover>")
(d-0)
(if (Appending?)
    (d-MoveCursorLeft))
(d-ChangeLonga)
(if (and (defined? 'Snippet::Longa) (> Snippet::Longa 0))
    (d-SelectSnippet Snippet::Longa)
    (d-CreateSnippetFromObject "Longa"))
  (if appending
    (d-MoveCursorRight))
	(if spillover 
		(d-SetPrefs "<spillover>1</spillover>")))
