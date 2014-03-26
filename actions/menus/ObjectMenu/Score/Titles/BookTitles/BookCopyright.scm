;;BookCopyright
(if BookCopyright::params
        (BookTitles::Do "Copyright" "copyright" BookCopyright::params #f)
        (BookTitles::Do "Copyright" "copyright" (string-append "Copyright © "  (if (getlogin) (getlogin) "") " " (strftime "%Y" (localtime (current-time))))  (_ "Give copyright notice or blank out to delete: ")))
