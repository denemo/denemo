;;BookTitle
(let ((tag "TopMargin")(edit (d-Directive-scoreheader? "BookTitle")))
    (if (equal? BookTitle::params "edit")
            (set! BookTitle::params #f))
    (if BookTitle::params
        (BookTitles::Do "Title" "title" BookTitle::params  #f edit)
        (let ((space (d-DirectiveGet-score-data tag)))
           (d-LilyPondInclude (cons 'delete "simplified-book-titling.ily"))
           (BookTitles::Do "Title" "title" (_ "My Title")  (_ "Give a title for the whole score or blank out to delete: ") edit)
           
           (if (not space)
                (set! space "0"))
           (set! space (DenemoGetUserNumberAsString (_ "Space Above Title") (_ "Give amount of space at top of title page") space))
           (if (not space)
                (set! space "0"))
           (d-SetSaved #f)
           (d-DirectivePut-score-prefix tag (string-append "#(define denemo-top-margin " space ")\n"))
           (d-DirectivePut-score-data tag space)
           (d-DirectivePut-score-override tag DENEMO_OVERRIDE_AFFIX))))

