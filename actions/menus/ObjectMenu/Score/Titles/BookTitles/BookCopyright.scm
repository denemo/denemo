;;BookCopyright
(let ((edit (d-Directive-scoreheader? "BookCopyright")))
    (if BookCopyright::params
        (BookTitles::Do "Copyright" "copyright" BookCopyright::params #f edit)
        (BookTitles::Do "Copyright" "copyright" (_ "My Copyright")  (_ "Give name for copyright etc or blank out to delete: ") edit)))
