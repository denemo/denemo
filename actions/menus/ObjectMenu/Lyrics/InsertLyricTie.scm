;InsertLyricTie
(let ((choice (RadioBoxMenu (cons (_ "Printing") "~") (cons (_ "Non Printing") "_"))))
 (if choice
    (begin
        (d-InsertTextInVerse choice)
        (d-SetSaved #f))))
