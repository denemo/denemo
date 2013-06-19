;;Longa
(d-0)
(d-MoveCursorLeft)
(d-ChangeLonga)
(d-MoveCursorRight)
(if (and (defined? 'Snippet::Longa) (> Snippet::Longa 0))
    (d-SelectSnippet Snippet::Longa)
    (d-CreateSnippetFromObject "Longa"))
