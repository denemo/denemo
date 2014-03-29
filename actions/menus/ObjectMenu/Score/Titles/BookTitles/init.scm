(define (BookTitles::Do field lilyfield initial help)
    (define tag (string-append "Book" field))
    (d-LilyPondInclude "book-titling.ily")
    (d-LilyPondInclude "simplified-book-titling.ily")
    (let ((chapter (if help (d-DirectiveGet-scoreheader-display tag) initial)))
  (if (not chapter)
    (set! chapter initial))
    (if help
     (set! chapter (d-GetUserInput initial help chapter #f)))
  (if chapter
   (begin
     (d-SetSaved #f)
     (if (string-null? chapter)
    (d-DirectiveDelete-scoreheader tag)
        (begin 
            (d-DirectivePut-scoreheader-display tag  chapter)
            (d-DirectivePut-scoreheader-override tag  (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
            (d-DirectivePut-scoreheader-postfix tag (string-append lilyfield " = \\markup { \\with-url #'\"scheme:(d-Book" field   ")\" "  "\"" chapter "\"}\n"))))))))

(define (BookTitles::DeleteInclude) ;;FIXME this should be part of the LilyPondInclude command
    (d-DirectiveDelete-score (string-append "LilyPondInclude:" "book-titling.ily"))
    (d-DirectiveDelete-score (string-append "LilyPondInclude:" "simplified-book-titling.ily")))
