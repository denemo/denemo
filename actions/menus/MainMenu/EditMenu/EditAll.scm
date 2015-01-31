;;;EditAll
(define-once EditSimilar::last #f)
(let ((choice #f))
    (if EditSimilar::last
        (let ((type (cdr EditSimilar::last)))
            (set! choice (RadioBoxMenu 
                    (cons (string-append (_ "Delete All ") type) 'delete)
                    (cons (string-append (_ "Execute Scheme on all ") type) 'execute))))
        (begin
            (set! choice 'menu)
            (d-WarningDialog (_ "You must choose a type of object to edit first"))))
    (if choice
        (begin
            (d-MoveToMovementBeginning)
            (d-EditSimilar choice))))
