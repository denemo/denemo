;;BookArranger
(let ((edit (d-Directive-scoreheader? "BookArranger")))
    (if BookArranger::params
        (BookTitles::Do "Arranger" "arranger" BookArranger::params #f edit)
        (BookTitles::Do "Arranger" "arranger" (_ "My Arranger")  (_ "Give name for arranger etc or blank out to delete: ") edit)))
        
