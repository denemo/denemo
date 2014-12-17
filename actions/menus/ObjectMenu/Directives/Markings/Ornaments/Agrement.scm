;;;agrements
(let ((choice (RadioBoxMenu 
                (cons "Tremblement" "tremblement")
                (cons "Pincé" "pince")
                (cons "Tremblement Appuyé" "tremblement_appuye")
                (cons "Suspension" "suspension"))))
    (if choice
        (d-Ornament (list (cons 'ornament choice)(cons 'direction "^")))))
