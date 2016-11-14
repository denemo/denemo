;;BookComposer
(let ((edit (d-Directive-scoreheader? "BookComposer")))
    (if BookComposer::params
        (BookTitles::Do "Composer" "composer" BookComposer::params #f edit)
        (BookTitles::Do "Composer" "composer" (_ "My Composer")  (_ "Give name for composer etc or blank out to delete: ") edit)))
