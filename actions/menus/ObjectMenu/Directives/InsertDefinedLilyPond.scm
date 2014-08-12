;;;InsertDefinedLilyPond
(let ((directives '()) (definitions #f) (choice #f))
    (define (get-second-line text)
        (let ((thelist (string-split text #\newline)))
            (if (> (length thelist) 1)
                (list-ref thelist 1)
                "")))
    (define (extract-menuitem tag)
        (define name (get-second-line tag))
        (cons name (lambda () (d-DirectivePut-standalone name) (d-DirectivePut-standalone-grob name  "TextScript") (d-DirectivePut-standalone-postfix name (string-append "\\" name " ")) (d-DirectivePut-standalone-display
        name name)(d-DirectivePut-standalone-minpixels name 30)(d-MoveCursorRight))))
        
    (set! directives (GetDefinitionDirectives))
    (if (not (null? directives))
        (set! definitions (map extract-menuitem directives)))
    (if definitions
        (set! choice (d-PopupMenu definitions))
        (d-WarningDialog (_ "No Definitions have been created for this score")))
    (if choice
        (begin  
            (choice)
            (d-RefreshDisplay)
            (d-SetSaved #f))))
