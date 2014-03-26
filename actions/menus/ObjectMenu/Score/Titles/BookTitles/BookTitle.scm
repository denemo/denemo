;;BookTitle
(if BookTitle::params
        (BookTitles::Do "Title" "title" BookTitle::params  #f)
        (BookTitles::Do "Title" "title" (_ "My Title")  (_ "Give a title for the whole score or blank out to delete: ")))
        
