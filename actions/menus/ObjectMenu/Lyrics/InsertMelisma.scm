;InsertMelisma
(let ((choice (RadioBoxMenu (cons (_ "During Word") " -- _ ") (cons (_ "End of Word") " __ _ ")    (cons (_ "Extend") " _ "))))
 (if choice
    (begin
        (d-InsertTextInVerse choice)
        (d-SetSaved #f))))
