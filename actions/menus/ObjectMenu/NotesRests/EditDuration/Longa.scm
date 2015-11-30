;;Longa
(let ((appending (or (None?) (Appending?))))
(d-0)
(if (Appending?)
    (d-MoveCursorLeft))
(d-ChangeLonga)
(if (and (defined? 'Snippet::Longa) (> Snippet::Longa 0))
    (d-SelectSnippet Snippet::Longa)
    (d-CreateSnippetFromObject "Longa"))
  (if appending
    (d-MoveCursorRight)))

