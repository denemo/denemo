;;Breve
(let ((appending (or (None?) (Appending?))))
(d-0)
(if (Appending?)
    (d-MoveCursorLeft))
(d-ChangeBreve);
(if (and (defined? 'Snippet::Breve) (> Snippet::Breve 0))
    (d-SelectSnippet Snippet::Breve)
    (d-CreateSnippetFromObject "Breve"))
  (if appending
    (d-MoveCursorRight)))

