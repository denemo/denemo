(define (BookTitles::Do field lilyfield initial help edit)
    (define tag (string-append "Book" field))
    (define (get-current tag)
        (let ((value (d-DirectiveGet-scoreheader-data tag)))
            (if value
                value
                (d-DirectiveGet-scoreheader-display tag)))) 
    (DenemoUseBookTitles)
    (if (equal? initial "edit")
        (eval-string (string-append "(d-" tag " #f)"))
        (let ((chapter (if help (get-current tag) initial))
                (simple-tag (string-append "Score" (string-capitalize field))))
            (define simple-title (get-current simple-tag)) 
            (if simple-title
                (begin
                    (d-DirectiveDelete-scoreheader simple-tag)
                    (set! chapter simple-title)))
            (if (not chapter)
                (set! chapter initial))
            (if help
                (set! chapter 
                    (if edit
                        (let ((response  (d-GetUserInputWithSnippets  (_ "Book Titles")  (string-append (_ "Give ") field) chapter)))
                            (if response
                                (car response)
                                #f))
                        (let ((response (d-GetUserInput initial help chapter)))
                            (if response
                                (string-append "\"" (scheme-escape response) "\"")
                                #f)))))
            (if chapter
                (begin
                 (d-SetSaved #f)
                 (if (string-null? chapter)
                    (d-DirectiveDelete-scoreheader tag)
                    (begin 
                        (d-DirectivePut-scoreheader-data tag  chapter)
                        (d-DirectivePut-scoreheader-display tag  (DenemoAbbreviatedString chapter))
                        (d-DirectivePut-scoreheader-override tag  (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
                        (d-DirectivePut-scoreheader-postfix tag (string-append lilyfield " = \\markup { \\with-url #'\"scheme:(d-Book" field   ")\" "  "{" chapter "}}\n")))))))))

