;;BookPoet
(let ((edit (d-Directive-scoreheader? "BookPoet")))
    (if BookPoet::params
        (BookTitles::Do "Poet" "poet" BookPoet::params #f edit)
        (BookTitles::Do "Poet" "poet" (_ "My Poet")  (_ "Give name for poet/lyricist/librettist etc or blank out to delete: ") edit)))
