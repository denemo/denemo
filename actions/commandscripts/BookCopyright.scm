;;BookCopyright
        (BookTitles::Do "Copyright" "copyright" (string-append "Copyright © "  (if (getlogin) (getlogin) "") " " (strftime "%Y" (localtime (current-time))))  "Give copyright notice or blank out to delete: ")
