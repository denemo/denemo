;; transpose output
(define start (d-GetNote))
(d-MoveCursorRight)
(define end (d-GetNote))

(if (or (string=? start "") (string=? end ""))
    (d-GetUserInput (_ "No Transposition Interval") (_ "To use this function you need to place the cursor\non the first of two notes which define\nthe transposition required") (_ "Ok"))
(d-StaffProperties (string-append "staff-prolog-insert \\transpose " start " " end)))

