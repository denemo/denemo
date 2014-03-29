;;BookTitle
(let ((tag "TopMargin"))
        (if BookTitle::params
                (BookTitles::Do "Title" "title" BookTitle::params  #f)
                (let ((space (d-DirectiveGet-score-data tag)))
                    (if (not space)
                        (set! space "6"))
                    (set! space (d-GetUserInput (_ "Space Above Title") (_ "Give amount of space at top of title page") space))
                    (if (string? space)
                        (begin
                            (BookTitles::DeleteInclude)
                            (d-DirectivePut-score-prefix tag (string-append "#(define denemo-top-margin " space ")\n"))
                            (d-DirectivePut-score-data tag space)
                            (d-DirectivePut-score-override tag DENEMO_OVERRIDE_AFFIX)
                            (BookTitles::Do "Title" "title" (_ "My Title")  (_ "Give a title for the whole score or blank out to delete: ")))))))
        
