;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;BookDate
(if BookDate::params
 	(BookTitles::Do "Date" "date" BookDate::params #f)
        (BookTitles::Do "Date" "date" (strftime "%Y" (localtime (current-time)))  (_ "Give date for score or blank out to delete: ")))
