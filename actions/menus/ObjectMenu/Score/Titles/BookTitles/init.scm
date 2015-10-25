(define (BookTitles::Do field lilyfield initial help)
    (define tag (string-append "Book" field))
    (define (get-current tag)
        (let ((value (d-DirectiveGet-scoreheader-data tag)))
            (if value
                value
                (d-DirectiveGet-scoreheader-display tag))))
    (d-LilyPondInclude "simplified-book-titling.ily")
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
                (set! chapter (d-GetUserInput initial help chapter)))
            (if chapter
                (begin
                 (d-SetSaved #f)
                 (if (string-null? chapter)
                (d-DirectiveDelete-scoreheader tag)
                    (begin 
                        (d-DirectivePut-scoreheader-data tag  chapter)
                        (d-DirectivePut-scoreheader-display tag  (DenemoAbbreviatedString chapter))
                        (d-DirectivePut-scoreheader-override tag  (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
                        (d-DirectivePut-scoreheader-postfix tag (string-append lilyfield " = \\markup { \\with-url #'\"scheme:(d-Book" field   ")\" "  "\"" chapter "\"}\n")))))))))

