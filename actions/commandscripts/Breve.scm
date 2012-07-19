;;Breve
(d-0)
(d-MoveCursorLeft)
(d-ChangeBreve)
(d-MoveCursorRight)
(if (and (defined? 'Snippet::Breve) (> Snippet::Breve 0))
    (d-SelectSnippet Snippet::Breve)
    (d-CreateSnippetFromObject "Breve"))

