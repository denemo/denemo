;;;ToggleCustomOrnament
(let ((directives '()) (definitions #f) (choice #f) (params ToggleCustomOrnament::params))
    (define (get-second-line text)
        (let ((thelist (string-split text #\newline)))
            (if (> (length thelist) 1)
                (list-ref thelist 1)
                "")))
    (define (extract-menuitem tag)
        (define name (get-second-line tag))
        (cons name (lambda () (ChordAnnotation 
                                (string-append "Toggle" (string-upcase name 0 1)) 
                                (string-append "\\" name) params LG-DownPrall)  )))
           
     (define (edit-ornament tag)
        (define data (eval-string (d-DirectiveGet-score-data (string-append "Allow\n" params))))
        (define choice (RadioBoxMenu
            (cons (_ "Edit this individual ornament") 'instance)
            (cons (_ "Edit the ornament definition") 'all)))
        (case choice
                ((instance) (d-InfoDialog (_ "Position editing not implemented")))
                ((all)
                     (d-CustomOrnamentDefinition data))))
                               
    (define (edit-directive tag)
        (define choice (RadioBoxMenu (cons (_ "Edit") 'edit)  (cons (_ "Delete") 'delete) (cons (_ "Advanced") 'advanced)))
        (case choice
            ((delete) (d-DirectiveDelete-chord tag))
            ((edit) (edit-ornament tag))
            ((advanced) (d-DirectiveTextEdit-chord tag))))

    (define (get-name togglename) ;;; get the name from the current chord
        (string-downcase (substring togglename (string-length "Toggle")) 0 1))


    (let loop ((count 1))
            (define good-tag (d-Directive-score? (string-append "Allow\n" (number->string count))))
            (if good-tag
                (begin
                    (set! directives (cons good-tag directives))
                    (loop (1+ count)))))
    (if (not (null? directives))
        (set! definitions (map extract-menuitem directives)))
    (if definitions
        (begin
            (if (string? params)
                (let ((tag (string-append "Toggle" (string-upcase params 0 1))))  
                    (set! choice (lambda () (edit-directive tag))))
                    
                (if (list? params) ;call ChordAnnotation with params  
                    (set! choice (lambda () (ChordAnnotation 
                                (string-append "Toggle" (string-upcase (get-name (car params)) 0 1)) 
                                (string-append "\\" (get-name (car params))) (cdr params) LG-DownPrall)  ))
                    (set! choice (d-PopupMenu definitions)))))
        (d-WarningDialog (_ "No Definitions have been created for this score")))
    (if choice
        (begin
            (choice)
            (d-RefreshDisplay)
            (d-SetSaved #f))))
