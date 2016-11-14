;;BookDate
(let ((edit (d-Directive-scoreheader? "BookDate")))
    (if BookDate::params
        (BookTitles::Do "Date" "date" BookDate::params #f edit)
        (BookTitles::Do "Date" "date" (_ "My Date")  (_ "Give name for date/opus number etc or blank out to delete: ") edit)))
