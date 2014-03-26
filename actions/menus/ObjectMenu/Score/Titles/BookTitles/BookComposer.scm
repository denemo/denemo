;;BookComposer
(if BookComposer::params
        (BookTitles::Do "Composer" "composer" BookComposer::params  #f)
        (BookTitles::Do "Composer" "composer"  (_ "My Composer")  (_ "Give name for composer or blank out to delete: ")))
