;;BookPoet
(if BookPoet::params
	(BookTitles::Do "Poet" "poet"  BookPoet::params #f)
	(BookTitles::Do "Poet" "poet" "My Poet"  "Give name for poet/lyricist/librettist etc or blank out to delete: "))
