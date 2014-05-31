;InsertMelisma
(let ((choice (RadioBoxMenu (cons (_ "Split Word") " -- ") (cons (_ "Extend Word") " __ "))))
 (if choice
    (begin
        (d-InsertTextInVerse choice)
        (d-SetSaved #f))))
